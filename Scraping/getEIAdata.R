# ------------------------------------------------------------------------------
# 1. SETUP & LIBRARIES
# ------------------------------------------------------------------------------
if (!require("httr")) install.packages("httr")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("dplyr")) install.packages("dplyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("tidyr")) install.packages("tidyr")
if (!require("purrr")) install.packages("purrr")
if (!require("zoo")) install.packages("zoo")
if (!require("quantmod")) install.packages("quantmod")

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)
library(zoo)
library(quantmod)

# --- CONFIGURATION ---
source("EIA_API.R")
start_date <- "2000-01-01"

# ------------------------------------------------------------------------------
# 2. DEFINING THE DATA DICTIONARY (CORRECTED IDs)
# ------------------------------------------------------------------------------
series_list <- list(
  # DAILY DATA (Prices)
  daily = list(
    list(name = "Price_WTI",      id = "RWTC",  route = "petroleum/pri/spt/data"),
    list(name = "Price_Brent",    id = "RBRTE", route = "petroleum/pri/spt/data")
  ),
  
  # WEEKLY DATA (Fundamentals)
  weekly = list(
    # Supply
    list(name = "Supply_Production", id = "WCRFPUS2", route = "petroleum/sum/sndw/data"),
    list(name = "Trade_Imports",     id = "WCRIMUS2", route = "petroleum/sum/sndw/data"),
    list(name = "Trade_Exports",     id = "WCREXUS2", route = "petroleum/sum/sndw/data"),
    
    # Refinery & Demand
    list(name = "Refinery_Util_Pct", id = "WPULEUS3", route = "petroleum/sum/sndw/data"),
    list(name = "Demand_Total",      id = "WRPUPUS2", route = "petroleum/sum/sndw/data"), # Total Product Supplied
    
    # Stocks (Corrected IDs)
    list(name = "Stocks_Comm_Crude", id = "WCESTUS1",            route = "petroleum/stoc/wstk/data"),
    list(name = "Stocks_Cushing",    id = "W_EPC0_SAX_YCUOK_MBBL", route = "petroleum/stoc/wstk/data"), # Fixed ID
    list(name = "Stocks_SPR",        id = "WCSSTUS1",            route = "petroleum/stoc/wstk/data"),
    list(name = "Stocks_Gasoline",   id = "WGTSTUS1",            route = "petroleum/stoc/wstk/data"),
    list(name = "Stocks_Distillate", id = "WDISTUS1",            route = "petroleum/stoc/wstk/data")
  )
)

# ------------------------------------------------------------------------------
# 3. DOWNLOAD FUNCTION
# ------------------------------------------------------------------------------
fetch_all_eia_data <- function(api_key, series_info, start_date, freq_setting) {
  
  base_url <- paste0("https://api.eia.gov/v2/", series_info$route, "/")
  all_data <- list()
  offset <- 0
  page_size <- 5000
  keep_fetching <- TRUE
  
  print(paste("Downloading:", series_info$name, "(", freq_setting, ")..."))
  
  while(keep_fetching) {
    params <- list(
      api_key = api_key,
      frequency = freq_setting,
      "data[0]" = "value",
      "facets[series][]" = series_info$id,
      "start" = start_date,
      "sort[0][column]" = "period",
      "sort[0][direction]" = "asc",
      offset = offset,
      length = page_size 
    )
    
    response <- tryCatch({
      GET(url = base_url, query = params)
    }, error = function(e) return(NULL))
    
    if (is.null(response) || status_code(response) != 200) {
      warning(paste("  Error fetching", series_info$name))
      break
    }
    
    json_content <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
    batch_data <- json_content$response$data
    
    if (is.null(batch_data) || length(batch_data) == 0) {
      keep_fetching <- FALSE
    } else {
      if (!is.data.frame(batch_data)) batch_data <- as.data.frame(batch_data)
      batch_clean <- batch_data %>% select(period, value)
      all_data[[length(all_data) + 1]] <- batch_clean
      
      if (nrow(batch_data) < page_size) {
        keep_fetching <- FALSE
      } else {
        offset <- offset + page_size
      }
    }
  }
  
  if (length(all_data) == 0) return(NULL)
  
  full_df <- bind_rows(all_data) %>%
    mutate(date = as.Date(period), value = as.numeric(value)) %>%
    select(date, value) %>%
    arrange(date) %>%
    distinct(date, .keep_all = TRUE)
  
  colnames(full_df)[2] <- series_info$name
  return(full_df)
}

# ------------------------------------------------------------------------------
# 4. EXECUTE DOWNLOAD (EIA)
# ------------------------------------------------------------------------------

# Download Daily
daily_list <- lapply(series_list$daily, function(x) fetch_all_eia_data(api_key, x, start_date, "daily"))
daily_list <- daily_list[!sapply(daily_list, is.null)]
df_daily_raw <- daily_list %>% reduce(full_join, by = "date")

# Download Weekly
weekly_list <- lapply(series_list$weekly, function(x) fetch_all_eia_data(api_key, x, start_date, "weekly"))
weekly_list <- weekly_list[!sapply(weekly_list, is.null)]
df_weekly_raw <- weekly_list %>% reduce(full_join, by = "date")

# Merge on Skeleton
date_skeleton <- data.frame(date = seq(from = as.Date(start_date), to = Sys.Date(), by = "day"))

master_df <- date_skeleton %>%
  left_join(df_daily_raw, by = "date") %>%
  left_join(df_weekly_raw, by = "date") %>%
  fill(-date, .direction = "down") %>%
  # Wir behalten erstmal alle Tage, um sauber Finanzdaten zu mergen
  filter(date >= as.Date(start_date))

# ------------------------------------------------------------------------------
# 5. FETCH FINANCIAL DATA (Yahoo)
# ------------------------------------------------------------------------------
print("Fetching VIX & DXY...")

# VIX
vix_xts <- getSymbols("^VIX", src = "yahoo", from = start_date, auto.assign = FALSE)
vix_df <- data.frame(date = index(vix_xts), VIX_Close = as.numeric(Cl(vix_xts)))

# Dollar Index (DXY) - Correct syntax handling
dxy_xts <- getSymbols("DX-Y.NYB", src = "yahoo", from = start_date, auto.assign = FALSE)
dxy_df <- data.frame(date = index(dxy_xts), DXY = as.numeric(Cl(dxy_xts)))

# Merge
final_df_hybrid <- master_df %>%
  left_join(vix_df, by = "date") %>%
  left_join(dxy_df, by = "date") %>%
  fill(VIX_Close, DXY, .direction = "down")

# ------------------------------------------------------------------------------
# 6. WEEKEND CLEANING & FEATURE ENGINEERING
#    Crucial: Filter weekends BEFORE calculating lags!
# ------------------------------------------------------------------------------
print("Cleaning Data & Calculating Features...")

model_df <- final_df_hybrid %>%
  # 1. Filter: Nur Wochentage (Mo=1 bis Fr=5) & Tage wo wir WTI Preise haben
  mutate(weekday = wday(date, week_start = 1)) %>%
  filter(weekday <= 5) %>%
  filter(!is.na(Price_WTI)) %>% # Entfernt Feiertage ohne Handel
  select(-weekday) %>%
  
  # 2. Sortieren für korrekte Lags
  arrange(date) %>%
  
  # 3. Features berechnen (Lag 5 = 1 Handelswoche)
  mutate(
    # --- TARGET ---
    Return_WTI_Daily = log(Price_WTI / lag(Price_WTI, 1)),
    
    # --- FEATURES ---
    # Momentum (Mo vs Mo)
    Lagged_Return_1W = log(Price_WTI / lag(Price_WTI, 5)),
    VIX_Level        = VIX_Close,
    DXY_Change       = log(DXY / lag(DXY, 5)),
    
    # Fundamentals (% Change vs last trading week)
    Chg_Production   = (Supply_Production - lag(Supply_Production, 5)) / lag(Supply_Production, 5),
    
    # Net Imports Calculation
    Net_Imports_Current = Trade_Imports - Trade_Exports,
    Net_Imports_Lagged  = lag(Trade_Imports, 5) - lag(Trade_Exports, 5),
    Delta_Net_Imports   = Net_Imports_Current - Net_Imports_Lagged,

    Chg_RefineryRun  = (Refinery_Util_Pct - lag(Refinery_Util_Pct, 5)) / lag(Refinery_Util_Pct, 5),
    Chg_Demand_Total = (Demand_Total - lag(Demand_Total, 5)) / lag(Demand_Total, 5),
    
    # Inventory Signals
    Chg_Stocks_Crude   = (Stocks_Comm_Crude - lag(Stocks_Comm_Crude, 5)) / lag(Stocks_Comm_Crude, 5),
    Chg_Stocks_Cushing = (Stocks_Cushing - lag(Stocks_Cushing, 5)) / lag(Stocks_Cushing, 5),
    Chg_Stocks_Gas     = (Stocks_Gasoline - lag(Stocks_Gasoline, 5)) / lag(Stocks_Gasoline, 5),
    Chg_Stocks_Dist    = (Stocks_Distillate - lag(Stocks_Distillate, 5)) / lag(Stocks_Distillate, 5),
    Chg_SPR            = (Stocks_SPR - lag(Stocks_SPR, 5)) / lag(Stocks_SPR, 5)
  ) %>%
  
  # 4. Remove NAs caused by Lags
  drop_na()

# ------------------------------------------------------------------------------
# 7. FINAL CHECK
# ------------------------------------------------------------------------------
print("Final Dataset ready for Hybrid Models:")
print(head(model_df))
print(paste("Total Rows:", nrow(model_df)))

# Save
write.csv(model_df, "Data/market_data.csv", row.names = FALSE)


print(head(model_df$date, 15))
