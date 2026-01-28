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

