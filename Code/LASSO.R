# ------------------------------------------------------------------------------
# 1. SETUP & LIBRARIES
# ------------------------------------------------------------------------------
if (!require("tm")) install.packages("tm")
if (!require("gamlr")) install.packages("gamlr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("lubridate")) install.packages("lubridate")
if (!require("Matrix")) install.packages("Matrix")

library(tm)
library(gamlr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(Matrix)

set.seed(123)

# ------------------------------------------------------------------------------
# 2. DATEN VORBEREITEN (Wie zuvor)
# ------------------------------------------------------------------------------

# A. TEXT
print("Lade Textdaten...")
df_speeches <- read.csv("Data/speeches.csv", stringsAsFactors = FALSE)
df_press    <- read.csv("Data/press_releases.csv", stringsAsFactors = FALSE)

colnames(df_speeches)[1:2] <- c("date", "text")
colnames(df_press)[1:2]    <- c("date", "text")

text_raw <- bind_rows(df_speeches %>% select(date, text), df_press %>% select(date, text))

text_daily <- text_raw %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(text = paste(text, collapse = " ")) %>%
  ungroup()

# B. MARKT
print("Lade Marktdaten...")
market_data <- read.csv("Data/market_data.csv", stringsAsFactors = FALSE) %>%
  mutate(date = as.Date(date))

# C. MERGE
df_final <- inner_join(text_daily, market_data, by = "date") %>%
  arrange(date) %>%
  na.omit()

print(paste("Datenpunkte:", nrow(df_final)))

# ------------------------------------------------------------------------------
# 3. TEXT MINING
# ------------------------------------------------------------------------------
print("Erstelle DTM...")
corpus <- VCorpus(VectorSource(df_final$text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)

dtm_sparse <- removeSparseTerms(DocumentTermMatrix(corpus), 0.98)
X_text <- as.matrix(dtm_sparse)

# ------------------------------------------------------------------------------
# 4. SPLIT & SETUP
# ------------------------------------------------------------------------------
Y_target <- df_final$Return_WTI_Daily

macro_cols <- c("Lagged_Return_1W", "VIX_Level", "DXY_Change", 
                "Chg_Production", "Delta_Net_Imports", "Chg_Demand_Total", 
                "Chg_Stocks_Crude", "Chg_Stocks_Cushing", "Chg_SPR")
existing_macro <- intersect(macro_cols, colnames(df_final))
X_macro <- as.matrix(df_final[, existing_macro])

# Indices
split_index <- floor(0.8 * nrow(df_final))
train_idx   <- 1:split_index
test_idx    <- (split_index + 1):nrow(df_final)

# ------------------------------------------------------------------------------
# 5. KORRIGIERTE PLOT-FUNKTION 
# ------------------------------------------------------------------------------

eval_one_step_ahead <- function(indices, predicted_returns, model_name, full_df) {
  
  subset_df <- full_df[indices, ]
  actual_prices <- subset_df$Price_WTI
  
  # FIX: Korrekte Berechnung des "Vortages-Preises"
  start_idx <- indices[1]
  
  if (start_idx == 1) {
    # Fall: In-Sample Start (Tag 1). 
    # Wir haben keinen Tag 0. Wir "schummeln" leicht und nehmen für Tag 1 
    # den echten Preis von Tag 1 als Basis (Fehler = 0 am Start),
    # da wir ohne Historie keine One-Step-Ahead Prognose für den allerersten Punkt machen können.
    prev_prices <- c(actual_prices[1], actual_prices[1:(length(actual_prices)-1)])
  } else {
    # Fall: Out-of-Sample (Tag N). Wir nehmen den Preis von Tag N-1.
    prev_prices <- c(full_df$Price_WTI[start_idx - 1], actual_prices[1:(length(actual_prices)-1)])
  }
  
  # Berechnung
  fitted_prices <- prev_prices * exp(predicted_returns)
  
  # MAE
  mae <- mean(abs(subset_df$Return_WTI_Daily - predicted_returns))
  
  # Plotting
  plot_df <- data.frame(
    Date = subset_df$date,
    Actual = actual_prices,
    Fitted = fitted_prices
  )
  
  p <- ggplot(plot_df, aes(x = Date)) +
    geom_line(aes(y = Actual, color = "Actual Price"), size = 0.6) +
    geom_line(aes(y = Fitted, color = "Fitted (One-Step-Ahead)"), size = 0.4, alpha = 0.8) +
    scale_color_manual(values = c("Actual Price" = "black", "Fitted (One-Step-Ahead)" = "red")) +
    labs(title = paste("One-Step-Ahead Forecast:", model_name),
         subtitle = paste("Based on previous day's actual price + predicted return.\nMAE (Returns):", round(mae, 5)),
         y = "WTI Price (USD)", x = "Date") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
}

# ------------------------------------------------------------------------------
# 6. MODELLE NEU KALIBRIEREN (Aggressiveres Tuning)
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


# --- 2. Hybrid (In-Sample) - DER WICHTIGSTE TEST ---
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


# --- 3. Hybrid (Out-of-Sample) ---
print("--- 3. Hybrid (Out-of-Sample) ---")

fit_hybrid_oos <- gamlr(X_hybrid[train_idx, ], Y_target[train_idx], 
                        free = 1:n_macro, lmr=1e-5, standardize = TRUE)

pred_hybrid_oos <- get_best_pred(fit_hybrid_oos, X_hybrid[test_idx, ])
eval_one_step_ahead(test_idx, pred_hybrid_oos, "Hybrid (OOS)", df_final)



