# ------------------------------------------------------------------------------
# Load Setup
# ------------------------------------------------------------------------------

source("Code/1_Setup.R")



# ------------------------------------------------------------------------------
# 7. ELASTIC NET (glmnet): Text-only + Hybrid, each IS and OOS
# ------------------------------------------------------------------------------

if (!require("glmnet")) install.packages("glmnet")
if (!require("slam")) install.packages("slam")
if (!require("Matrix")) install.packages("Matrix")

library(glmnet)
library(slam)
library(Matrix)

# -------------------------
# 7.0 Robust conversion: DocumentTermMatrix -> sparse dgCMatrix
# -------------------------
dtm_triplet <- as.simple_triplet_matrix(dtm_sparse)

# safest dims source
dims_safe <- dim(dtm_sparse)  # c(#documents, #terms)

# indices/values
i <- as.integer(dtm_triplet$i)
j <- as.integer(dtm_triplet$j)
x <- as.numeric(dtm_triplet$v)

# guard: sometimes indices can be 0-based
if (min(i, na.rm = TRUE) == 0) i <- i + 1
if (min(j, na.rm = TRUE) == 0) j <- j + 1

# drop broken entries
ok <- !is.na(i) & !is.na(j) & !is.na(x) &
  i >= 1 & j >= 1 &
  i <= dims_safe[1] & j <= dims_safe[2]

i <- i[ok]; j <- j[ok]; x <- x[ok]

# build sparse matrix (add dimnames only if valid)
X_text_sp <- sparseMatrix(
  i = i, j = j, x = x,
  dims = dims_safe
)

dn <- dtm_triplet$dimnames
if (is.list(dn) && length(dn) == 2 &&
    length(dn[[1]]) == dims_safe[1] &&
    length(dn[[2]]) == dims_safe[2]) {
  dimnames(X_text_sp) <- dn
}

print("--- X_text_sp CHECK ---")
print(class(X_text_sp))
print(dim(X_text_sp))

# -------------------------
# 7.A Hybrid design matrix + penalty factor
# -------------------------
# Macro is dense; cbind with sparse returns sparse-friendly matrix
X_hybrid_sp <- cbind(X_macro, X_text_sp)

# Unpenalize macro (0), penalize text (1) — analogous to free=1:n_macro in gamlr
penalty_hybrid <- c(rep(0, ncol(X_macro)), rep(1, ncol(X_text_sp)))

# Sanity check to avoid glmnet penalty mismatch
if (length(penalty_hybrid) != ncol(X_hybrid_sp)) {
  stop(paste0("penalty_hybrid length (", length(penalty_hybrid),
              ") != ncol(X_hybrid_sp) (", ncol(X_hybrid_sp), ")."))
}

# -------------------------
# 7.B Helper: Time-Series Correct CV for Elastic Net
# -------------------------
fit_enet_predict <- function(X_train, y_train, X_test,
                             alphas = c(0.1, 0.5, 0.9),
                             penalty.factor = NULL,
                             standardize = TRUE,
                             nfolds = 5) {
  
  if (is.null(penalty.factor)) penalty.factor <- rep(1, ncol(X_train))
  
  # TIME SERIES FIX: Erstelle Folds, die die Zeit respektieren (Block CV)
  # Wir teilen die Daten der Reihe nach in 5 Blöcke, statt zufällig zu mischen.
  n_train <- nrow(X_train)
  foldid <- ceiling(seq(1, nfolds, length.out = n_train))
  
  best <- list(cvm = Inf)
  
  for (a in alphas) {
    # Wir übergeben 'foldid' statt 'nfolds'
    cv <- cv.glmnet(
      x = X_train, y = y_train,
      alpha = a,
      family = "gaussian",
      foldid = foldid, # Zwingt Time-Series Blöcke
      standardize = standardize,
      penalty.factor = penalty.factor
    )
    
    cvm_min <- min(cv$cvm)
    
    if (cvm_min < best$cvm) {
      best$cvm <- cvm_min
      best$alpha <- a
      best$cv <- cv
    }
  }
  
  # Prediction
  pred <- as.numeric(predict(best$cv, newx = X_test, s = "lambda.min"))
  list(pred = pred, alpha = best$alpha, lambda = best$cv$lambda.min, cv = best$cv)
}

# -------------------------
# 7.1 Elastic Net — Text Only (In-Sample)
# -------------------------
print("--- Elastic Net: Text Only (In-Sample) ---")

enet_text_is <- fit_enet_predict(
  X_train = X_text_sp, y_train = Y_target,
  X_test  = X_text_sp,
  alphas  = c(0.1, 0.5, 0.9),
  standardize = TRUE
)

eval_one_step_ahead(
  indices = 1:nrow(df_final),
  predicted_returns = enet_text_is$pred,
  model_name = paste0("Elastic Net Text (IS) alpha=", enet_text_is$alpha,
                      " lambda=", signif(enet_text_is$lambda, 3)),
  full_df = df_final
)

# -------------------------
# 7.2 Elastic Net — Text Only (Out-of-Sample)
# -------------------------
print("--- Elastic Net: Text Only (Out-of-Sample) ---")

enet_text_oos <- fit_enet_predict(
  X_train = X_text_sp[train_idx, ], y_train = Y_target[train_idx],
  X_test  = X_text_sp[test_idx, ],
  alphas  = c(0.1, 0.5, 0.9),
  standardize = TRUE
)

eval_one_step_ahead(
  indices = test_idx,
  predicted_returns = enet_text_oos$pred,
  model_name = paste0("Elastic Net Text (OOS) alpha=", enet_text_oos$alpha,
                      " lambda=", signif(enet_text_oos$lambda, 3)),
  full_df = df_final
)

# -------------------------
# 7.3 Elastic Net — Hybrid (Text + Numerical) (In-Sample)
# -------------------------
print("--- Elastic Net: Hybrid (In-Sample) ---")

enet_hybrid_is <- fit_enet_predict(
  X_train = X_hybrid_sp, y_train = Y_target,
  X_test  = X_hybrid_sp,
  alphas  = c(0.1, 0.5, 0.9),
  penalty.factor = penalty_hybrid,   # macro unpenalized
  standardize = TRUE
)

eval_one_step_ahead(
  indices = 1:nrow(df_final),
  predicted_returns = enet_hybrid_is$pred,
  model_name = paste0("Elastic Net Hybrid (IS) alpha=", enet_hybrid_is$alpha,
                      " lambda=", signif(enet_hybrid_is$lambda, 3)),
  full_df = df_final
)

# Macro coefficient check (intercept + macro)
coefs_hybrid_is <- as.matrix(coef(enet_hybrid_is$cv, s = "lambda.min"))
print("--- Elastic Net Hybrid (IS): Macro Coefficients ---")
print(coefs_hybrid_is[1:(ncol(X_macro) + 1), , drop = FALSE])

# -------------------------
# 7.4 Elastic Net — Hybrid (Text + Numerical) (Out-of-Sample)
# -------------------------
print("--- Elastic Net: Hybrid (Out-of-Sample) ---")

enet_hybrid_oos <- fit_enet_predict(
  X_train = X_hybrid_sp[train_idx, ], y_train = Y_target[train_idx],
  X_test  = X_hybrid_sp[test_idx, ],
  alphas  = c(0.1, 0.5, 0.9),
  penalty.factor = penalty_hybrid,
  standardize = TRUE
)

eval_one_step_ahead(
  indices = test_idx,
  predicted_returns = enet_hybrid_oos$pred,
  model_name = paste0("Elastic Net Hybrid (OOS) alpha=", enet_hybrid_oos$alpha,
                      " lambda=", signif(enet_hybrid_oos$lambda, 3)),
  full_df = df_final
)
