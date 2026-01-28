# ------------------------------------------------------------------------------
# Load Setup
# ------------------------------------------------------------------------------

source("1_Setup.R")

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

# ------------------------------------------------------------------------------
# 11. FINAL COMPARISON TABLE (Optional but helpful)
# ------------------------------------------------------------------------------
print("--- FINAL MAE COMPARISON (OOS) ---")

# Helper to calc MAE quickly
calc_mae <- function(pred) mean(abs(df_final$Return_WTI_Daily[test_idx] - pred))

maes <- data.frame(
  Model = c("Naive Benchmark", 
            "Lasso Text", "Lasso Hybrid",
            "ElasticNet Text", "ElasticNet Hybrid",
            "RF Text", "RF Hybrid",
            "XGB Text", "XGB Hybrid"),
  
  MAE_OOS = c(calc_mae(pred_naive_oos),
              # Falls Variablen aus vorherigen Blöcken existieren, hier eintragen:
              if(exists("pred_text_oos")) calc_mae(pred_text_oos) else NA,
              if(exists("pred_hybrid_oos")) calc_mae(pred_hybrid_oos) else NA,
              if(exists("enet_text_oos")) calc_mae(enet_text_oos$pred) else NA,
              if(exists("enet_hybrid_oos")) calc_mae(enet_hybrid_oos$pred) else NA,
              if(exists("pred_text_oos")) calc_mae(pred_text_oos) else NA, # RF Variable names checken
              if(exists("pred_hybrid_oos")) calc_mae(pred_hybrid_oos) else NA,
              if(exists("pred_xgb_text_oos")) calc_mae(pred_xgb_text_oos) else NA,
              if(exists("pred_xgb_hyb_oos")) calc_mae(pred_xgb_hyb_oos) else NA
  )
)

print(maes %>% arrange(MAE_OOS))