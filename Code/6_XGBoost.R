# ------------------------------------------------------------------------------
# Load Setup
# ------------------------------------------------------------------------------

source("Code/1_Setup.R")

# ------------------------------------------------------------------------------
# 9. XGBOOST (Gradient Boosting)
#    Runs: Text-only (IS/OOS) and Hybrid (IS/OOS)
#    Features: Time-series tuning, Early Stopping, DMatrix format
# ------------------------------------------------------------------------------

if (!require("xgboost")) install.packages("xgboost")
library(xgboost)

print("--- STARTING XGBOOST MODELS ---")

# -------------------------
# 9.1 Setup & Tuning Grid
# -------------------------

# Wir nutzen dieselben internen Validation-Splits wie beim Random Forest
# um Hyperparameter zu tunen, ohne OOS-Daten zu berühren.
ntr <- length(train_idx)
val_start <- floor(0.8 * ntr)
train_sub <- train_idx[1:val_start]
val_sub   <- train_idx[(val_start + 1):ntr]

# Hyperparameter Grid (klein gehalten für Speed)
xgb_grid <- expand.grid(
  topK      = c(200, 500),   # Wie viele Wörter?
  eta       = c(0.01, 0.05), # Lernrate (kleiner ist robuster)
  max_depth = c(3, 6)        # Baumtiefe (kleiner verhindert Overfitting)
)

# Helper: XGBoost braucht sein eigenes Datenformat (DMatrix)
# Wir definieren eine Funktion zum Tunen

tune_xgboost <- function(is_hybrid = FALSE) {
  
  best_params <- list(score = Inf)
  
  for(i in 1:nrow(xgb_grid)) {
    # 1. Feature Selection (Top K Wörter)
    k <- xgb_grid$topK[i]
    tmp <- build_X_text_topK(k) # Nutzt Funktion aus RF Section
    Xk  <- tmp$X
    
    # Text Only oder Hybrid?
    if (is_hybrid) {
      data_mat <- cbind(X_macro, Xk)
    } else {
      data_mat <- Xk
    }
    
    # 2. DMatrix erstellen für Train-Sub und Val-Sub
    dtrain_sub <- xgb.DMatrix(data = data_mat[train_sub, ], label = Y_target[train_sub])
    dval_sub   <- xgb.DMatrix(data = data_mat[val_sub, ],   label = Y_target[val_sub])
    
    # 3. Parameter setzen
    params <- list(
      objective = "reg:squarederror",
      eta = xgb_grid$eta[i],
      max_depth = xgb_grid$max_depth[i],
      subsample = 0.7,        # Stochastisches Boosting
      colsample_bytree = 0.7
    )
    
    # 4. Training mit Early Stopping
    # Wir trainieren max 1000 Runden, aber stoppen wenn Val-Error 50 Runden nicht fällt
    set.seed(123)
    model_cv <- xgb.train(
      params = params,
      data = dtrain_sub,
      nrounds = 1000,
      watchlist = list(val = dval_sub),
      early_stopping_rounds = 50,
      verbose = 0 # Silent mode
    )
    
    # Score checken (Best Iteration RMSE)
    current_score <- model_cv$best_score
    
    if(current_score < best_params$score) {
      best_params <- list(
        score = current_score,
        grid_row = xgb_grid[i, ],
        nrounds = model_cv$best_iteration,
        terms = tmp$terms
      )
    }
  }
  return(best_params)
}

# -------------------------
# 9.2 XGBoost: TEXT ONLY
# -------------------------
print("--- XGBoost: Tuning Text Only ---")
best_xgb_text <- tune_xgboost(is_hybrid = FALSE)

print("Best XGB Text Params:")
print(best_xgb_text$grid_row)

# Finales Training auf dem GANZEN Train Set mit besten Parametern
X_text_best_xgb <- as.matrix(dtm_sparse[, best_xgb_text$terms])

dtrain_full <- xgb.DMatrix(data = X_text_best_xgb[train_idx, ], label = Y_target[train_idx])
dtest_full  <- xgb.DMatrix(data = X_text_best_xgb[test_idx, ],  label = Y_target[test_idx])
dall_full   <- xgb.DMatrix(data = X_text_best_xgb,              label = Y_target) # Für IS Prediction

final_params_text <- list(
  objective = "reg:squarederror",
  eta = best_xgb_text$grid_row$eta,
  max_depth = best_xgb_text$grid_row$max_depth,
  subsample = 0.7,
  colsample_bytree = 0.7
)

set.seed(123)
xgb_text_model <- xgb.train(
  params = final_params_text,
  data = dtrain_full,
  nrounds = best_xgb_text$nrounds,
  verbose = 0
)

# --- CHART 1: Text Only (In-Sample) ---
pred_xgb_text_is <- predict(xgb_text_model, dall_full)
eval_one_step_ahead(
  indices = 1:nrow(df_final),
  predicted_returns = pred_xgb_text_is,
  model_name = "XGBoost Text (IS)",
  full_df = df_final
)

# --- CHART 2: Text Only (Out-of-Sample) ---
pred_xgb_text_oos <- predict(xgb_text_model, dtest_full)
eval_one_step_ahead(
  indices = test_idx,
  predicted_returns = pred_xgb_text_oos,
  model_name = "XGBoost Text (OOS)",
  full_df = df_final
)


# -------------------------
# 9.3 XGBoost: HYBRID
# -------------------------
print("--- XGBoost: Tuning Hybrid ---")
best_xgb_hybrid <- tune_xgboost(is_hybrid = TRUE)

print("Best XGB Hybrid Params:")
print(best_xgb_hybrid$grid_row)

# Finales Training Hybrid
X_text_best_h_xgb <- as.matrix(dtm_sparse[, best_xgb_hybrid$terms])
X_hybrid_best_xgb <- cbind(X_macro, X_text_best_h_xgb)

dtrain_hyb <- xgb.DMatrix(data = X_hybrid_best_xgb[train_idx, ], label = Y_target[train_idx])
dtest_hyb  <- xgb.DMatrix(data = X_hybrid_best_xgb[test_idx, ],  label = Y_target[test_idx])
dall_hyb   <- xgb.DMatrix(data = X_hybrid_best_xgb,              label = Y_target)

final_params_hyb <- list(
  objective = "reg:squarederror",
  eta = best_xgb_hybrid$grid_row$eta,
  max_depth = best_xgb_hybrid$grid_row$max_depth,
  subsample = 0.7,
  colsample_bytree = 0.7
)

set.seed(123)
xgb_hybrid_model <- xgb.train(
  params = final_params_hyb,
  data = dtrain_hyb,
  nrounds = best_xgb_hybrid$nrounds,
  verbose = 0
)

# --- CHART 3: Hybrid (In-Sample) ---
pred_xgb_hyb_is <- predict(xgb_hybrid_model, dall_hyb)
eval_one_step_ahead(
  indices = 1:nrow(df_final),
  predicted_returns = pred_xgb_hyb_is,
  model_name = "XGBoost Hybrid (IS)",
  full_df = df_final
)

# --- CHART 4: Hybrid (Out-of-Sample) ---
pred_xgb_hyb_oos <- predict(xgb_hybrid_model, dtest_hyb)
eval_one_step_ahead(
  indices = test_idx,
  predicted_returns = pred_xgb_hyb_oos,
  model_name = "XGBoost Hybrid (OOS)",
  full_df = df_final
)

# -------------------------
# 9.4 Feature Importance (Hybrid)
# -------------------------
print("--- XGBoost Hybrid: Feature Importance ---")

# Importance Matrix berechnen
importance_matrix <- xgb.importance(model = xgb_hybrid_model)
# Wir zeigen die Top 15 Features (Gain = wichtigste Metrik für Vorhersagekraft)
print(xgb.plot.importance(importance_matrix, top_n = 15, measure = "Gain"))

# Falls du die reine Tabelle willst:
print(head(importance_matrix, 15))





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
















