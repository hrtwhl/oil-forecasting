# ------------------------------------------------------------------------------
# Load Setup
# ------------------------------------------------------------------------------

source("Code/1_Setup.R")

# ------------------------------------------------------------------------------
# 10. NAIVE BENCHMARK (Predict 0% Return)
#     The baseline: "Tomorrow's price will be exactly today's price."
#     Mathematically: Predicted Return = 0.
# ------------------------------------------------------------------------------

print("--- RUNNING NAIVE BENCHMARK ---")

# --- 10.1 Naive In-Sample ---
# Wir sagen für JEDEN Tag im Datensatz einfach 0.0 Return voraus.
pred_naive_is <- rep(0, nrow(df_final))

eval_one_step_ahead(
  indices = 1:nrow(df_final),
  predicted_returns = pred_naive_is,
  model_name = "Naive Benchmark (IS) - Predict 0",
  full_df = df_final
)

# --- 10.2 Naive Out-of-Sample ---
# Wir sagen für jeden Tag im Test-Set 0.0 Return voraus.
pred_naive_oos <- rep(0, length(test_idx))

eval_one_step_ahead(
  indices = test_idx,
  predicted_returns = pred_naive_oos,
  model_name = "Naive Benchmark (OOS) - Predict 0",
  full_df = df_final
)

