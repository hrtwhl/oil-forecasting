# ------------------------------------------------------------------------------
# Load Setup
# ------------------------------------------------------------------------------

source("1_Setup.R")


# ------------------------------------------------------------------------------
# 8. RANDOM FOREST (ranger) — IMPROVED VERSION (focus: better OOS, less overfitting)
#    Runs: Text-only (IS/OOS) and Hybrid (IS/OOS)
#    Key changes vs before:
#      - time-series validation inside TRAIN to tune hyperparameters (no leakage)
#      - stronger regularization: min.node.size, max.depth, sample.fraction, mtry
#      - tune TopK (text dimensionality) instead of fixing it
# ------------------------------------------------------------------------------

if (!require("ranger")) install.packages("ranger")
if (!require("slam")) install.packages("slam")
library(ranger)
library(slam)

set.seed(123)

# -------------------------
# 8.0 Build Top-K text matrix helper (dense, manageable for RF)
# -------------------------
term_freq <- slam::col_sums(dtm_sparse)

build_X_text_topK <- function(k) {
  k <- min(k, length(term_freq))
  top_terms <- names(sort(term_freq, decreasing = TRUE))[1:k]
  Xk <- as.matrix(dtm_sparse[, top_terms])
  list(X = Xk, terms = top_terms)
}

# -------------------------
# 8.1 Time-series validation split INSIDE training (no look-ahead)
# -------------------------
ntr <- length(train_idx)
val_start <- floor(0.8 * ntr)
train_sub <- train_idx[1:val_start]
val_sub   <- train_idx[(val_start + 1):ntr]

# -------------------------
# 8.2 Tuning grid (keep it small + regularizing)
# -------------------------
grid <- expand.grid(
  topK            = c(200, 300, 500),
  min.node.size   = c(10, 20, 50),
  max.depth       = c(5, 8, 12),
  mtry_frac       = c(0.05, 0.10, 0.20),   # mtry = frac * p
  sample.fraction = c(0.60, 0.80)
)

# -------------------------
# 8.3 Tune TEXT-ONLY RF on internal validation
# -------------------------
print("--- RF Tuning: TEXT-ONLY (using internal time-series validation) ---")

best_text <- list(mae = Inf)

for (g in 1:nrow(grid)) {
  k  <- grid$topK[g]
  mns <- grid$min.node.size[g]
  md  <- grid$max.depth[g]
  sf  <- grid$sample.fraction[g]
  
  tmp <- build_X_text_topK(k)
  Xk  <- tmp$X
  
  p <- ncol(Xk)
  mtry <- max(1, floor(grid$mtry_frac[g] * p))
  
  rf <- ranger(
    y = Y_target[train_sub],
    x = data.frame(Xk[train_sub, ]),
    num.trees = 1000,
    mtry = mtry,
    min.node.size = mns,
    max.depth = md,
    sample.fraction = sf,
    seed = 123
  )
  
  pred_val <- predict(rf, data.frame(Xk[val_sub, ]))$predictions
  mae_val  <- mean(abs(Y_target[val_sub] - pred_val))
  
  if (mae_val < best_text$mae) {
    best_text <- list(
      mae = mae_val,
      params = grid[g, ],
      terms = tmp$terms
    )
  }
}

print("--- BEST TEXT RF PARAMS (by validation MAE) ---")
print(best_text$params)
print(paste("Validation MAE:", round(best_text$mae, 5)))

# Fit best TEXT model on FULL TRAIN and predict IS + OOS
X_text_best <- as.matrix(dtm_sparse[, best_text$terms])
p_text <- ncol(X_text_best)
mtry_text <- max(1, floor(best_text$params$mtry_frac * p_text))

rf_text_best <- ranger(
  y = Y_target[train_idx],
  x = data.frame(X_text_best[train_idx, ]),
  num.trees = 2000,
  mtry = mtry_text,
  min.node.size = best_text$params$min.node.size,
  max.depth = best_text$params$max.depth,
  sample.fraction = best_text$params$sample.fraction,
  seed = 123,
  importance = "permutation"
)

# TEXT IS
pred_text_is <- predict(rf_text_best, data.frame(X_text_best))$predictions
eval_one_step_ahead(
  indices = 1:nrow(df_final),
  predicted_returns = pred_text_is,
  model_name = paste0("RF Text (IS) tuned, TopK=", ncol(X_text_best)),
  full_df = df_final
)

# TEXT OOS
pred_text_oos <- predict(rf_text_best, data.frame(X_text_best[test_idx, ]))$predictions
eval_one_step_ahead(
  indices = test_idx,
  predicted_returns = pred_text_oos,
  model_name = paste0("RF Text (OOS) tuned, TopK=", ncol(X_text_best)),
  full_df = df_final
)

# Optional: top importances for presentation
print("--- RF Text: Top 15 Variable Importances ---")
print(sort(rf_text_best$variable.importance, decreasing = TRUE)[1:15])

# -------------------------
# 8.4 Tune HYBRID RF on internal validation
# -------------------------
print("--- RF Tuning: HYBRID (using internal time-series validation) ---")

best_hybrid <- list(mae = Inf)

for (g in 1:nrow(grid)) {
  k  <- grid$topK[g]
  mns <- grid$min.node.size[g]
  md  <- grid$max.depth[g]
  sf  <- grid$sample.fraction[g]
  
  tmp <- build_X_text_topK(k)
  Xk  <- tmp$X
  Xhy <- cbind(X_macro, Xk)
  
  p <- ncol(Xhy)
  mtry <- max(1, floor(grid$mtry_frac[g] * p))
  
  rf <- ranger(
    y = Y_target[train_sub],
    x = data.frame(Xhy[train_sub, ]),
    num.trees = 1000,
    mtry = mtry,
    min.node.size = mns,
    max.depth = md,
    sample.fraction = sf,
    seed = 123
  )
  
  pred_val <- predict(rf, data.frame(Xhy[val_sub, ]))$predictions
  mae_val  <- mean(abs(Y_target[val_sub] - pred_val))
  
  if (mae_val < best_hybrid$mae) {
    best_hybrid <- list(
      mae = mae_val,
      params = grid[g, ],
      terms = tmp$terms
    )
  }
}

print("--- BEST HYBRID RF PARAMS (by validation MAE) ---")
print(best_hybrid$params)
print(paste("Validation MAE:", round(best_hybrid$mae, 5)))

# Fit best HYBRID model on FULL TRAIN and predict IS + OOS
X_text_best_h <- as.matrix(dtm_sparse[, best_hybrid$terms])
X_hybrid_best <- cbind(X_macro, X_text_best_h)

p_hyb <- ncol(X_hybrid_best)
mtry_hyb <- max(1, floor(best_hybrid$params$mtry_frac * p_hyb))

rf_hybrid_best <- ranger(
  y = Y_target[train_idx],
  x = data.frame(X_hybrid_best[train_idx, ]),
  num.trees = 2000,
  mtry = mtry_hyb,
  min.node.size = best_hybrid$params$min.node.size,
  max.depth = best_hybrid$params$max.depth,
  sample.fraction = best_hybrid$params$sample.fraction,
  seed = 123,
  importance = "permutation"
)

# HYBRID IS
pred_hybrid_is <- predict(rf_hybrid_best, data.frame(X_hybrid_best))$predictions
eval_one_step_ahead(
  indices = 1:nrow(df_final),
  predicted_returns = pred_hybrid_is,
  model_name = paste0("RF Hybrid (IS) tuned, TopK=", ncol(X_text_best_h)),
  full_df = df_final
)

# HYBRID OOS
pred_hybrid_oos <- predict(rf_hybrid_best, data.frame(X_hybrid_best[test_idx, ]))$predictions
eval_one_step_ahead(
  indices = test_idx,
  predicted_returns = pred_hybrid_oos,
  model_name = paste0("RF Hybrid (OOS) tuned, TopK=", ncol(X_text_best_h)),
  full_df = df_final
)

# Optional: top importances for presentation
print("--- RF Hybrid: Top 20 Variable Importances ---")
print(sort(rf_hybrid_best$variable.importance, decreasing = TRUE)[1:20])