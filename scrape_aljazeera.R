library(RSelenium)
library(wdman)
library(netstat)

# Get available geckodriver versions
binman::list_versions("geckodriver")

# Set up RSelenium with Firefox
port <- netstat::free_port()
driver <<- rsDriver(browser = "firefox",
                    geckover="0.35.0",
                    chromever=NULL,
                    check = FALSE,
                    port = port,
                    verbose = TRUE
)

remote_driver <- driver[["client"]]

# Navigate to the page
remote_driver$navigate("https://www.aljazeera.com/where/bangladesh/")

# Allow the page to load
Sys.sleep(5)

clickAjaxButton <- function(times, button_selector, delay = 5000) {
  for (i in 1:times) {
    # Try to locate the button
    button <- remote_driver$findElement(using = "css selector", paste0("", button_selector))
    
    if (!is.null(button)) {
      # Click the button
      button$clickElement()
      
      # Wait for the content to load
      Sys.sleep(delay / 1000) # Convert delay to seconds
    } else {
      cat("Button not found!\n")
      break
    }
  }
}

# Click the button 20 times to load more articles
clickAjaxButton(times = 22, button_selector = "#news-feed-container > button")



# Get all article URLs
article_links <- remote_driver$findElements(using = "css selector", ".u-clickable-card__link")
article_urls <- sapply(article_links, function(link) link$getElementAttribute("href"))
article_urls <- article_urls[1:229]

cat("Collected URLs:\n")
print(article_urls)

scrapeArticleDetails <- function(url) {
  cat(sprintf("Processing %s\n", url))
  
  # Navigate to the article page
  remote_driver$navigate(url)
  # Sys.sleep(2)
  
  # Initialize variables to store scraped data
  time_text <- NA
  content_text <- NA
  
  # Scrape publication time
  tryCatch({
    time_element <- remote_driver$findElement(using = "css selector", ".article-dates > .date-simple > span[aria-hidden='true']")
    time_text <- time_element$getElementText()
  }, error = function(e) {
    message("Time element not found for URL: ", url)
  })
  
  # Scrape content
  tryCatch({
    # Locate the <div> element using XPath
    div_element <- remote_driver$findElement(
      using = "xpath",
      '/html/body/div[1]/div/div[3]/div/div/div/div[1]/main/div[2]'
    )
    
    # Check if div_element is found (optional for debugging)
    if (length(div_element) > 0) {
      cat("Div element found.\n")
    }
    
    # Find all <p> elements inside the located <div>
    p_elements <- div_element$findChildElements(using = "xpath", ".//p")
    
    if (length(p_elements) == 0) {
      stop("No <p> elements found inside the div.")
    }
    p_elements[[1]]$getElementText()
    
    cat("Found", length(p_elements), "paragraphs.\n")
    
    # Concatenate text from all <p> elements
    content_text <- sapply(p_elements, function(p) p$getElementText())
    
    # Combine all the text from each <p> element into one single string
    content_text <- paste(content_text, collapse = " ")
    
  }, error = function(e) {
    message("Error locating <div> or <p> elements.")
  })
  
  return(list(url = url, time = time_text, content = content_text))
}



article_details <- lapply(article_urls, scrapeArticleDetails)


# Extract the relevant information from each article in the list
article_data <- lapply(article_details, function(article) {
  # Ensure that each element exists before extracting it
  list(
    url = article$url[[1]],       # Extract the URL from the first element
    time = article$time[[1]],     # Extract the time from the first element
    content = article$content     # Extract the content directly
  )
})

# Convert the list of lists into a data frame
articles_df <- do.call(rbind, lapply(article_data, as.data.frame))

# Convert all columns to character to ensure consistency
articles_df[] <- lapply(articles_df, as.character)

# Print the resulting data frame
print(article_data)
head(articles_df)

# Remove rows with NA values
articles <- na.omit(articles_df)
head(articles)

library(lubridate)


articles$time <- strptime(articles$time, format = "%d %b %Y")
articles$year <- year(ymd(articles$time))
articles$month <- month(ymd(articles$time))

write.csv(articles, "scraped_articles_df.csv", row.names = FALSE)



# Close the browser and stop the server
remote_driver$close()
driver$server$stop()

