library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)
library(writexl)
library(chromote)

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

# Define the years and their corresponding URLs
#target_urls <- list(
  #"2025" = "https://www.opec.org/speeches.html",
  #"2024" = "https://www.opec.org/speeches-2024.html"
#)

# 1. Das aktuelle Jahr (Sonderfall)
target_urls <- list(
  "2025" = "https://www.opec.org/speeches.html",
  "2024" = "https://www.opec.org/speeches-2024.html"
)


base_domain <- "https://www.opec.org"

# ------------------------------------------------------------------------------
# Helper Functions
# ------------------------------------------------------------------------------

# Extract speech links from a main year page
get_speech_links <- function(page_url) {
  message(paste("Scanning list page:", page_url))
  
  tryCatch({
    sess <- read_html_live(page_url)
    Sys.sleep(3)
    
    links <- sess %>%
      html_elements("a") %>%
      html_attr("href") %>%
      discard(is.na) %>%
      keep(~ str_detect(.x, "speech-detail")) %>%
      unique()
    
    rm(sess)
    gc()
    
    # Clean URL construction
    # Remove leading dots or slashes to avoid double slashes
    clean_links <- str_remove(links, "^\\./") %>% str_remove("^/")
    full_links <- ifelse(str_starts(clean_links, "http"), clean_links, paste0(base_domain, "/", clean_links))
    
    return(full_links)
    
  }, error = function(e) {
    message(paste("Error reading list page:", page_url))
    message(e)
    return(character(0))
  })
}

# Scrape the content of a single speech
get_speech_content <- function(speech_url) {
  
  tryCatch({
    sess <- read_html_live(speech_url)
    Sys.sleep(2)
    
    # 1. Extract and Fix Date from URL
    # Capture the date part after the ID number
    raw_date_string <- str_match(speech_url, "speech-detail/\\d+-(.*?)\\.html")[,2]
    
    # FIX: Handle date ranges (e.g., "18-19-january")
    # If the string starts with "number-number-", remove the second number
    clean_date_string <- str_replace(raw_date_string, "^(\\d+)-\\d+-", "\\1-")
    
    parsed_date <- dmy(clean_date_string)
    
    # 2. Extract Speech Text
    text_content <- sess %>%
      html_elements("p") %>%
      html_text2() %>%
      str_trim() %>%
      keep(~ nchar(.x) > 0) %>%
      paste(collapse = "\n\n")
    
    rm(sess)
    gc()
    
    return(tibble(
      date = parsed_date,
      speech = text_content,
      url = speech_url
    ))
    
  }, error = function(e) {
    message(paste("Failed to scrape speech:", speech_url))
    return(tibble(date = NA, speech = NA, url = speech_url))
  })
}

# ------------------------------------------------------------------------------
# Main Execution
# ------------------------------------------------------------------------------

# 1. Collect all individual speech URLs
all_speech_urls <- map(target_urls, get_speech_links) %>% unlist() %>% unique()

message(paste("Found", length(all_speech_urls), "speeches in total."))

# 2. Scrape content for each URL
final_data <- map_dfr(all_speech_urls, function(url) {
  message(paste("Scraping:", url))
  get_speech_content(url)
})

# 3. Clean up
final_data <- final_data %>%
  filter(!is.na(speech)) %>%
  arrange(desc(date))

# 4. Save to Excel
output_filename <- "opec_speeches.xlsx"
write_xlsx(final_data, output_filename)



library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)
library(readr) # Changed from writexl to readr for CSV support
# NOTE: Ensure you have installed 'chromote' -> install.packages("chromote")

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

# 1. Start with current year
target_urls <- list(
  "2025" = "https://www.opec.org/speeches.html"
)

# 2. Define years 2024 to 2003
years <- 2024:2003

# 3. Generate and name URLs
archive_urls <- setNames(
  as.list(paste0("https://www.opec.org/speeches-", years, ".html")),
  as.character(years)
)

# 4. Combine lists
target_urls <- c(target_urls, archive_urls)

base_domain <- "https://www.opec.org"

# Set locale to English to ensure 'October', 'May' etc. are parsed correctly
Sys.setlocale("LC_TIME", "C")

# ------------------------------------------------------------------------------
# Helper Functions
# ------------------------------------------------------------------------------

#' Extract speech links from a main year page
get_speech_links <- function(page_url) {
  message(paste("Scanning list page (Live Browser):", page_url))
  
  tryCatch({
    sess <- read_html_live(page_url)
    Sys.sleep(3)
    
    links <- sess %>%
      html_elements("a") %>%
      html_attr("href") %>%
      discard(is.na) %>%
      keep(~ str_detect(.x, "speech-detail")) %>%
      unique()
    
    try(sess$session$close(), silent = TRUE)
    rm(sess)
    gc()
    
    clean_links <- str_remove(links, "^\\./") %>% str_remove("^/")
    full_links <- ifelse(str_starts(clean_links, "http"), clean_links, paste0(base_domain, "/", clean_links))
    
    return(full_links)
    
  }, error = function(e) {
    message(paste("Error reading list page:", page_url))
    message(e)
    return(character(0))
  })
}

#' Scrape the content of a single speech
get_speech_content <- function(speech_url) {
  
  Sys.sleep(0.5)
  
  tryCatch({
    sess <- read_html_live(speech_url)
    Sys.sleep(1)
    
    # 1. Extract and Fix Date
    raw_date_string <- str_match(speech_url, "speech-detail/\\d+-(.*?)\\.html")[,2]
    
    # Clean date logic
    clean_string <- str_replace(raw_date_string, "^(\\d+)-\\d+-", "\\1-")
    clean_string <- str_replace_all(clean_string, "(\\d+)(st|nd|rd|th)", "\\1")
    clean_string <- str_replace(clean_string, "-202$", "-2020")
    
    parsed_date <- dmy(clean_string)
    
    # 2. Extract Speech Text with De-duplication
    paragraphs <- sess %>%
      html_elements("p") %>%
      html_text2() %>%
      str_trim() %>%
      keep(~ nchar(.x) > 0)
      
    # Remove duplicate paragraphs (common in footers/menus)
    # This helps prevent the text from exploding in size due to boilerplate
    paragraphs <- unique(paragraphs)
    
    text_content <- paste(paragraphs, collapse = "\n\n")
    
    try(sess$session$close(), silent = TRUE)
    rm(sess)
    gc() 
    
    return(tibble(
      date = parsed_date,
      speech = text_content,
      url = speech_url
    ))
    
  }, error = function(e) {
    message(paste("Failed to scrape speech:", speech_url, "-", e$message))
    return(tibble(date = NA, speech = NA, url = speech_url))
  })
}

# ------------------------------------------------------------------------------
# Main Execution
# ------------------------------------------------------------------------------

all_speech_urls <- map(target_urls, get_speech_links) %>% unlist() %>% unique()

message(paste("Found", length(all_speech_urls), "speeches in total. Starting scrape..."))

results_list <- list()
pb <- txtProgressBar(min = 0, max = length(all_speech_urls), style = 3)

for (i in seq_along(all_speech_urls)) {
  url <- all_speech_urls[i]
  
  res <- tryCatch({
    get_speech_content(url)
  }, error = function(e) {
    Sys.sleep(5) 
    get_speech_content(url)
  })
  
  results_list[[i]] <- res
  setTxtProgressBar(pb, i)
  
  if (i %% 10 == 0) gc()
}

close(pb)

final_data <- bind_rows(results_list)

final_data <- final_data %>%
  filter(!is.na(speech)) %>%
  arrange(desc(date))

# 4. Save to CSV (Excel Compatible)
# We use CSV because Excel .xlsx files have a hard limit of 32,767 characters per cell.
# CSVs do not have this limit. You can double-click this file to open it in Excel.
output_filename <- "opec_speeches_2003_2025.csv"
write_excel_csv(final_data, output_filename)

message(paste("Done! Scraped", nrow(final_data), "speeches."))
message(paste("File saved as:", output_filename))


