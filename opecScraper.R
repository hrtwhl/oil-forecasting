library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)
library(writexl)
# library(httr) # No longer needed as we use the live browser
# NOTE: Ensure you have installed 'chromote' -> install.packages("chromote")

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

# Define the years and their corresponding URLs
target_urls <- list(
  "2025" = "https://www.opec.org/speeches.html",
  "2024" = "https://www.opec.org/speeches-2024.html"
)

base_domain <- "https://www.opec.org"

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

#' Scrape the content of a single speech
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

message(paste("Found", length(all_speech_urls), "speeches in total. Starting scrape..."))

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
output_filename <- "opec_speeches_2024_2025.xlsx"
write_xlsx(final_data, output_filename)

message(paste("Done! Scraped", nrow(final_data), "speeches."))
message(paste("File saved as:", output_filename))


