reddit_scraper <- function(addition = c(""), full_url = paste(c("https://www.reddit.com/"), addition, sep = ""), missing = c(NA)){
      
      #Load (and, if necessary, install) libraries
      if (!all(c("rvest", "dplyr") %in% installed.packages()[,1])){
            
            install.packages("rvest")
            install.packages("dplyr")
      }
      
      library(rvest)
      library(dplyr)
      
      #Get the site url as specified by "url" argument
      
            ##Add "/r" string into "full_url" to make the url correct
      if(nchar(addition) != 0){
            full_url <- gsub(".com", ".com/r", full_url) 
      }
      
      full_url_html <- read_html(full_url)
      
      #Get titles of posts on the main page
      title_text <- html_nodes(full_url_html, ".title.may-blank") %>% html_text()
      
      #Get votes numbers
      votes <- html_nodes(full_url_html, "div.score.unvoted") %>% html_text()
      votes <- as.vector(votes)
      votes <- gsub("•", missing, votes)
      
      #Get webpage
      webpage <- html_nodes(full_url_html, "span.domain") %>% html_text()
      
      #Make a data frame
      reddit_data <- data.frame(title = title_text, votes = votes, webpage = webpage, time = Sys.time())
      
      #Remove NAs if full_url = "https://www.reddit.com/"
      if(nchar(full_url) == 23){
            reddit_data <- na.omit(reddit_data)
            row.names(reddit_data) <- seq(1, along.with = reddit_data$votes)
      }
      
      
      View(reddit_data)
      
}