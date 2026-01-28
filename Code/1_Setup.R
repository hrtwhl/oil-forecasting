# ------------------------------------------------------------------------------
# 1. SETUP & LIBRARIES
# ------------------------------------------------------------------------------
if (!require("tm")) install.packages("tm")
if (!require("gamlr")) install.packages("gamlr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("lubridate")) install.packages("lubridate")
if (!require("Matrix")) install.packages("Matrix")
# NEU: Für Side-by-Side Charts
if (!require("gridExtra")) install.packages("gridExtra")

library(tm)
library(gamlr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(Matrix)
library(gridExtra)

set.seed(123)

# ------------------------------------------------------------------------------
# 2. DATEN VORBEREITEN (KORRIGIERT: Tägliche Zeitreihe behalten)
# ------------------------------------------------------------------------------

# A. TEXT DATEN LADEN & AGGREGIEREN
print("Lade Textdaten...")
df_speeches <- read.csv("Data/speeches.csv", stringsAsFactors = FALSE)
df_press    <- read.csv("Data/press_releases.csv", stringsAsFactors = FALSE)

# Spaltennamen vereinheitlichen
colnames(df_speeches)[1:2] <- c("date", "text")
colnames(df_press)[1:2]    <- c("date", "text")

# Zusammenfügen
text_raw <- bind_rows(
  df_speeches %>% select(date, text),
  df_press %>% select(date, text)
)

# Auf Tagesbasis aggregieren
text_daily <- text_raw %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(text = paste(text, collapse = " ")) %>%
  ungroup()

# B. MARKT DATEN LADEN
print("Lade Marktdaten...")
market_data <- read.csv("Data/market_data.csv", stringsAsFactors = FALSE) %>%
  mutate(date = as.Date(date))

# C. MERGE (FIX: LEFT JOIN für lückenlose Zeitreihe)
# ------------------------------------------------------------------------------
# Wir nutzen left_join auf market_data, damit Tage OHNE Text nicht gelöscht werden.
df_final <- left_join(market_data, text_daily, by = "date") %>%
  arrange(date) %>%
  
  # WICHTIG: NA im Text (keine Rede) durch Platzhalter ersetzen
  # Sonst stürzt die DTM-Erstellung ab.
  mutate(text = ifelse(is.na(text), "no_text_event", text)) %>%
  
  # Bereinigung ungültiger Preise/Returns
  filter(!is.na(Price_WTI) & Price_WTI > 0) %>%
  filter(is.finite(Return_WTI_Daily))

print(paste("Finale Datenpunkte (Daily):", nrow(df_final)))
# Die Zahl sollte jetzt deutlich höher sein (ca. 5000+ Zeilen)

# ------------------------------------------------------------------------------
# 3. TEXT MINING
# ------------------------------------------------------------------------------

print("Erstelle DTM...")

# Corpus erstellen
corpus <- VCorpus(VectorSource(df_final$text))

# Preprocessing
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)

dtm <- DocumentTermMatrix(corpus)

# WICHTIG: Bei Daily Data sind 80% der Zeilen leer. 
# Ein 'removeSparseTerms(0.98)' würde fast alles löschen.
# Wir setzen es auf 0.999 (Wort muss in mind. 0.1% der Tage vorkommen -> ca. 5 Tage bei 5000 Zeilen)
dtm_sparse <- removeSparseTerms(dtm, 0.999)

# Wir entfernen explizit den Platzhalter "notextevent", falls er durch Stemming entstanden ist
# (kann auch "no_text_event" sein, je nach Stemming)
# Wir suchen den Index des Platzhalters
terms_vec <- colnames(dtm_sparse)
dummy_idx <- grep("no_text_event", terms_vec) # oder "notextevent" nach Stemming checken

if(length(dummy_idx) > 0) {
  # Wir entfernen diese Spalte, da sie keine Info enthält (ist ja nur der Platzhalter)
  dtm_sparse <- dtm_sparse[, -dummy_idx] 
}

X_text <- as.matrix(dtm_sparse)
print(paste("Text-Matrix Dimension:", dim(X_text)[1], "Zeilen x", dim(X_text)[2], "Wörter"))

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
# 5. DUAL-CHART EVALUATION (LINKS: Returns/Professor, RECHTS: Price/Trading)
# ------------------------------------------------------------------------------

eval_one_step_ahead <- function(indices, predicted_returns, model_name, full_df) {
  
  # --- DATEN VORBEREITEN ---
  subset_df <- full_df[indices, ]
  actual_prices <- subset_df$Price_WTI
  actual_returns <- subset_df$Return_WTI_Daily
  
  # One-Step-Ahead Preis Berechnung (Anker setzen)
  start_idx <- indices[1]
  if (start_idx == 1) {
    # In-Sample Start: Tag 1 Preis als Basis
    prev_prices <- c(actual_prices[1], actual_prices[1:(length(actual_prices)-1)])
  } else {
    # Out-of-Sample Start: Tag N-1 Preis als Basis
    prev_prices <- c(full_df$Price_WTI[start_idx - 1], actual_prices[1:(length(actual_prices)-1)])
  }
  
  fitted_prices <- prev_prices * exp(predicted_returns)
  
  # MAE auf Returns berechnen (Wissenschaftlich korrekt)
  mae <- mean(abs(actual_returns - predicted_returns))
  
  # Plotting DataFrame
  plot_df <- data.frame(
    Date = subset_df$date,
    Actual_Price = actual_prices,
    Fitted_Price = fitted_prices,
    Actual_Return = actual_returns,
    Predicted_Return = predicted_returns
  )
  
  # --- CHART 1 (LINKS): RAW RETURNS (Professor Style) ---
  # Zeigt das "Rauschen" und wie schwer es ist, die Richtung vorherzusagen.
  p1 <- ggplot(plot_df, aes(x = Date)) +
    geom_line(aes(y = Actual_Return, color = "Actual Return"), alpha = 1, size = 0.3) +
    geom_line(aes(y = Predicted_Return, color = "Predicted Return"), alpha = 0.8, size = 0.4) +
    scale_color_manual(values = c("Actual Return" = "#00589C", "Predicted Return" = "#50E3C2")) +
    labs(title = paste("Raw Signal (Returns):", model_name),
         subtitle = paste("MAE (Error):", round(mae, 5), "\n(Prediction vs. Daily Volatility)"),
         y = "Log Return", x = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 10, face = "bold"),
          axis.title = element_text(size = 8),
          legend.text = element_text(size = 7),
          legend.title = element_blank())
  
  # --- CHART 2 (RECHTS): PRICE TRACKER (Dein Style) ---
  # Zeigt die praktische Anwendung (One-Step-Ahead).
  p2 <- ggplot(plot_df, aes(x = Date)) +
    geom_line(aes(y = Actual_Price, color = "Actual Price"), size = 0.6) +
    geom_line(aes(y = Fitted_Price, color = "One-Step-Ahead Forecast"), size = 0.4, alpha = 0.8) +
    scale_color_manual(values = c("Actual Price" = "#00589C", "One-Step-Ahead Forecast" = "#50E3C2")) +
    labs(title = paste("Price Tracker:", model_name),
         subtitle = "Re-anchored daily to previous price.\nShows ability to track trend.",
         y = "WTI Price (USD)", x = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 10, face = "bold"),
          axis.title = element_text(size = 8),
          legend.text = element_text(size = 7),
          legend.title = element_blank())
  
  # --- NEBENEINANDER PLOTTEN ---
  grid.arrange(p1, p2, ncol = 2)
}







