
# ------------------------------------------------------------------------------
# Load Setup
# ------------------------------------------------------------------------------

source("Code/1_Setup.R")

# ------------------------------------------------------------------------------
# Model Calibration
# ------------------------------------------------------------------------------

# Hilfsfunktion für Prediction
get_best_pred <- function(model, newdata) {
  # Wir nehmen das Modell mit dem MINIMALEN AICc (Konservativ)
  # Wenn das zu flach ist, könnte man select="1se" nehmen, aber min ist meist ok für LASSO
  best_idx <- which.min(summary(model)$aicc)
  predict(model, newdata, select = best_idx)[,1]
}

# --- 1. Text Only (In-Sample) ---
print("--- 1. Text Only (In-Sample) ---")
# standardize = FALSE oft besser bei DTMs, da TF-IDF schon skaliert ist. 
# Aber bei reinen Counts ist TRUE besser. Wir testen standardize=FALSE für Text.
fit_text_is <- gamlr(X_text, Y_target, lmr=1e-4) # Sehr kleine Penalty erlaubt mehr Varianz

pred_text_is <- get_best_pred(fit_text_is, X_text)
eval_one_step_ahead(1:nrow(df_final), pred_text_is, "Text Only (IS)", df_final)

# --- 2. Text Only (Out-of-Sample) ---
print("--- 4. Text Only (Out-of-Sample) ---")

fit_text_oos <- gamlr(X_text[train_idx, ], Y_target[train_idx], lmr=1e-4)

pred_text_oos <- get_best_pred(fit_text_oos, X_text[test_idx, ])
eval_one_step_ahead(test_idx, pred_text_oos, "Text Only (OOS)", df_final)


# --- 3. Hybrid (In-Sample) - DER WICHTIGSTE TEST ---
print("--- 2. Hybrid (In-Sample) ---")

X_hybrid <- cbind(X_macro, X_text)
n_macro <- ncol(X_macro)

# free = 1:n_macro: Zwingt LASSO, die Makro-Daten IMMER zu nehmen (keine Penalty).
# standardize = TRUE: Wichtig, weil Returns (0.01) und VIX (20.0) unterschiedliche Skalen haben!
fit_hybrid_is <- gamlr(X_hybrid, Y_target, free = 1:n_macro, lmr=1e-5, standardize = TRUE)

pred_hybrid_is <- get_best_pred(fit_hybrid_is, X_hybrid)
eval_one_step_ahead(1:nrow(df_final), pred_hybrid_is, "Hybrid (IS)", df_final)

# DIAGNOSE: Was sind die Koeffizienten der Macro-Variablen?
print("--- MACRO COEFFICIENTS CHECK ---")
best_idx <- which.min(summary(fit_hybrid_is)$aicc)
all_coefs <- coef(fit_hybrid_is, select = best_idx)
# Zeige nur die Macro-Koeffizienten (die ersten n_macro + Intercept)
print(as.matrix(all_coefs)[1:(n_macro+1), , drop=FALSE])


# --- 4. Hybrid (Out-of-Sample) ---
print("--- 3. Hybrid (Out-of-Sample) ---")

fit_hybrid_oos <- gamlr(X_hybrid[train_idx, ], Y_target[train_idx], 
                        free = 1:n_macro, lmr=1e-5, standardize = TRUE)

pred_hybrid_oos <- get_best_pred(fit_hybrid_oos, X_hybrid[test_idx, ])
eval_one_step_ahead(test_idx, pred_hybrid_oos, "Hybrid (OOS)", df_final)