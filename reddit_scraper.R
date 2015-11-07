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
      reddit_data <- data.frame(title = title_text, votes = votes, webpage = webpage, time = Sys.time(), stringsAsFactors = FALSE)
      filename <- paste("reddit_", addition, sep = "")
      #assign(reddit_data, filename)
      ### WHAT NOW: how to assign new things to that new variable ("reddit_addition")???
      
      
      #Remove NAs if full_url = "https://www.reddit.com/"
      if(nchar(full_url) == 23){
            reddit_data <- na.omit(reddit_data)
            row.names(reddit_data) <- seq(1, along.with = reddit_data$votes)
      }
      
      
      if(file.exists(filename)){
            y_n <- readline("File for that subreddit exists, add data? Y/N ")
            if(y_n == "Y" | y_n == "y"){
                  tempfile <- read.csv(filename)
                  newfile <- rbind(reddit_data, tempfile)
                  write.csv(newfile, filename, row.names = FALSE)
            }
      }else{
            write.csv(reddit_data, file = filename, row.names = FALSE)
      }
      
      
      
      data_y_n <- readline("View downloaded data? Y/N ")
      
      if(data_y_n == "Y" | data_y_n == "y"){
            View(reddit_data)
      }
}