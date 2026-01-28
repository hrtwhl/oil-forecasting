library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)
library(readr)
library(chromote)
library(tesseract) 
library(magick)    

# ------------------------------------------------------------------------------
# 1. Configuration & Setup
# ------------------------------------------------------------------------------

# Define target URLs: 2025 (current) + 2024 back to 2002 (archive)
target_urls <- list("2025" = "https://www.opec.org/press-releases.html")

years <- 2024:2002
archive_urls <- setNames(
  as.list(paste0("https://www.opec.org/press-releases-", years, ".html")),
  as.character(years)
)

target_urls <- c(target_urls, archive_urls)
base_domain <- "https://www.opec.org"

# Force English locale for date parsing
Sys.setlocale("LC_TIME", "C")

# Initialize OCR Engine
# 'preserve_interword_spaces = 1' helps keep table columns visually separated
ocr_engine <- tesseract("eng", options = list(preserve_interword_spaces = 1))

# ------------------------------------------------------------------------------
# 2. Helper Functions
# ------------------------------------------------------------------------------

#' Extract all press release links from a main list page
get_pr_links <- function(page_url) {
  message(paste("Scanning list page:", page_url))
  
  tryCatch({
    sess <- read_html_live(page_url)
    Sys.sleep(3) # Wait for JavaScript to render the list
    
    links <- sess %>%
      html_elements("a") %>%
      html_attr("href") %>%
      discard(is.na) %>%
      keep(~ str_detect(.x, "pr-detail")) %>%
      unique()
    
    # Cleanup session immediately
    try(sess$session$close(), silent = TRUE)
    rm(sess); gc()
    
    # Clean and build full URLs
    clean_links <- str_remove(links, "^\\./") %>% str_remove("^/")
    full_links <- ifelse(str_starts(clean_links, "http"), clean_links, paste0(base_domain, "/", clean_links))
    
    return(full_links)
    
  }, error = function(e) {
    message(paste("Error reading list page:", page_url))
    return(character(0))
  })
}

#' Extract text from a single image URL using OCR
extract_text_from_image <- function(img_url) {
  tryCatch({
    img <- image_read(img_url)
    
    # 1. Size Filter: Skip tiny icons/spacers that cause errors
    info <- image_info(img)
    if(info$width < 100 || info$height < 50) {
      return(NA) 
    }
    
    # 2. Pre-processing: Grayscale & Contrast for better number recognition
    img <- img %>%
      image_convert(type = 'Grayscale') %>%
      image_modulate(brightness = 100, saturation = 0, hue = 100) %>%
      image_contrast(sharpen = 1)
    
    # 3. Run Tesseract
    text <- ocr(img, engine = ocr_engine)
    return(str_trim(text))
    
  }, error = function(e) return(NA))
}

#' Scrape the content (Text + HTML Tables + Image Tables) of a single PR
get_pr_content <- function(pr_url) {
  
  Sys.sleep(0.5) # Polite delay
  
  tryCatch({
    sess <- read_html_live(pr_url)
    Sys.sleep(1) # Wait for dynamic content
    
    # --- A. Date Extraction ---
    raw_date <- str_match(pr_url, "pr-detail/\\d+-(.*?)\\.html")[,2]
    # Clean standard date patterns
    clean_date <- str_replace(raw_date, "^(\\d+)-\\d+-", "\\1-") %>% 
                  str_replace_all("(\\d+)(st|nd|rd|th)", "\\1")
    parsed_date <- dmy(clean_date)
    
    # --- B. Text & HTML Table Extraction ---
    # We select both paragraphs <p> and tables <table>
    content_nodes <- sess %>% html_elements("p, table") 
    
    content_text_list <- map_chr(content_nodes, function(node) {
      if (html_name(node) == "table") {
        # Format HTML tables with explicit markers
        tbl_text <- html_text2(node)
        return(paste0("\n\n[HTML TABLE DATA START]\n", tbl_text, "\n[HTML TABLE DATA END]\n\n"))
      } else {
        # Standard text
        return(html_text2(node))
      }
    })
    
    main_text <- paste(content_text_list, collapse = "\n\n") %>% str_trim()
    
    # --- C. Image OCR Extraction ---
    img_nodes <- sess %>% html_elements("img")
    img_srcs <- html_attr(img_nodes, "src")
    
    # Filter for valid content images (ignore logos/icons)
    valid_imgs <- img_srcs %>%
      discard(is.na) %>%
      keep(~ str_detect(.x, "\\.(png|jpg|jpeg)$")) %>%
      discard(~ str_detect(tolower(.x), "logo|icon|button|menu|footer|spacer|separator"))
    
    valid_imgs_full <- ifelse(str_starts(valid_imgs, "http"), valid_imgs, 
                              paste0(base_domain, "/", str_remove(valid_imgs, "^/")))
    
    ocr_data <- ""
    
    if(length(valid_imgs_full) > 0) {
      extracted <- map_chr(valid_imgs_full, function(x) {
        txt <- extract_text_from_image(x)
        
        # Only keep if substantial text was found (>10 chars)
        if(!is.na(txt) && nchar(txt) > 10) {
          return(paste0("\n\n[[IMAGE TABLE DATA START]]\nSource: ", basename(x), "\n", txt, "\n[[IMAGE TABLE DATA END]]"))
        } else {
          return("")
        }
      })
      ocr_data <- paste(extracted, collapse = "\n")
    }
    
    # Cleanup
    try(sess$session$close(), silent = TRUE)
    rm(sess); gc()
    
    # --- D. Final Combine ---
    full_content <- paste(main_text, ocr_data, sep = "\n")
    
    return(tibble(
      date = parsed_date, 
      content = full_content, 
      url = pr_url
    ))
    
  }, error = function(e) {
    message(paste("Failed to scrape:", pr_url))
    return(tibble(date = NA, content = NA, url = pr_url))
  })
}

# ------------------------------------------------------------------------------
# 3. Main Execution
# ------------------------------------------------------------------------------

# A. Collect all Links
message("Phase 1: Collecting all press release URLs (2002-2025)...")
all_pr_urls <- map(target_urls, get_pr_links) %>% unlist() %>% unique()

message(paste("Found", length(all_pr_urls), "press releases in total."))
message("Phase 2: Scraping content (Text, HTML Tables, and OCR Images)...")

# B. Scrape Content
results_list <- list()
pb <- txtProgressBar(min = 0, max = length(all_pr_urls), style = 3)

for (i in seq_along(all_pr_urls)) {
  url <- all_pr_urls[i]
  
  # Retry logic for stability
  res <- tryCatch({
    get_pr_content(url)
  }, error = function(e) {
    Sys.sleep(5) 
    get_pr_content(url) 
  })
  
  results_list[[i]] <- res
  setTxtProgressBar(pb, i)
  
  # Aggressive garbage collection to manage memory during long OCR run
  if (i %% 5 == 0) gc() 
}

close(pb)

# C. Process and Save
final_data <- bind_rows(results_list) %>%
  filter(!is.na(content)) %>%
  arrange(desc(date))

output_filename <- "opec_press_releases_full_OCR.csv"
write_excel_csv(final_data, output_filename)

message(paste("Scraping Complete!"))
message(paste("Saved", nrow(final_data), "releases to:", output_filename))



# Test
target_release <- final_data %>% 
  filter(date == as.Date("2025-10-01"))

# 2. Print the full content to the console
cat(target_release$content)

