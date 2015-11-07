reddit_scraper_all <- function(url){
      
      #Load libraries
      library(rvest)
      library(dplyr)
      
      #Get the site url as specified by "url" argumentz
      url <- read_html(url)
      
      #Get titles of posts on the main page
      title_text <- html_nodes(url, ".title.may-blank") %>% html_text()

      
      #Get votes numbers
      votes <- html_nodes(url, "div.score.unvoted") %>% html_text()
      votes <- as.vector(votes)
      
      
      #Get webpage
      webpage <- html_nodes(url, "span.domain") %>% html_text()
      
      
      #Make a data frame
      reddit_data <- data.frame(title = title_text, votes = votes, webpage = webpage)
      View(reddit_data)
      
}