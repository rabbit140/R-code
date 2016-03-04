twitter_scraper <- function(searchTerm = readline("Enter search term: "),
                            nTweets = as.numeric(readline("Enter number of tweets to scrape: ")),
                            min_freq =  as.numeric(readline("Enter minimum frequency of words for wordcloud: ")),
                            max_words = as.numeric(readline("Enter maximum number of words for wordcloud: "))){
      
      #Load necessary libraries
      library(twitteR)
      library(httr)
      library(tm)
      library(wordcloud)
      
      #Get needed credentials
      key <- "ypUHX4XfEXjovqc89ZRyrrXg3"
      secret <- "CquCl4sxScFk9LA5QR2StRgiU8SEV3hd3pYY4yvnbkecz5emiU"
      token <- "472293226-7Vrzna7jdiES1u5LcfPcke9d0pr1om7MbvOv4Xor"
      token_secret <- "GPyfk4GtrYWeg5v3arzUm4Fl4K5IwpAjhpele2Cu6ERSc"
      
      #Authenticate Twitter with OAuth
      suppressMessages(setup_twitter_oauth(key, secret, token, token_secret))
      
      #Scrape n tweets
      tweets <- searchTwitter(searchTerm, n = nTweets)
      
      #Make a list of tweets
      tweetslist <- sapply(tweets, function(x) x$getText())
      
      #Make a corpus and transform it
      tweetscorpus <- Corpus(VectorSource(tweetslist))
      toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
      tweetscorpus <- tm_map(tweetscorpus, toSpace, "/")
      tweetscorpus <- tm_map(tweetscorpus, toSpace, "@")
      tweetscorpus <- tm_map(tweetscorpus, toSpace, "\\|")
      tweetscorpus <- tm_map(tweetscorpus, toSpace, "https")
      tweetscorpus <- tm_map(tweetscorpus, content_transformer(tolower))
      tweetscorpus <- tm_map(tweetscorpus, removeNumbers)
      tweetscorpus <- tm_map(tweetscorpus, function(x) removeWords(x, stopwords()))
      tweetscorpus <- tm_map(tweetscorpus, removePunctuation)
      tweetscorpus <- tm_map(tweetscorpus, stripWhitespace)
      tweetscorpus <- tm_map(tweetscorpus, function(x) iconv(enc2utf8(x$content), sub = "bytes"))
      tweetscorpus <- tm_map(tweetscorpus, function(x) removeWords(x, c("tco")))
      tweetscorpus <- tm_map(tweetscorpus, PlainTextDocument)
      
      #Color function
      cols <- colorRampPalette(c("steelblue", "gold"))
      cols_n <- sqrt(length(tweetscorpus))
      pdf_yn <- readline("Save wordcloud to pdf file? (Y/N): ")
      
      #Write to pdf?
      if(pdf_yn == "Y" ){
            file_name <- readline("Enter file name: ")
            pdf(file = paste(file_name, ".pdf", sep = ""))
            wordcloud(tweetscorpus, min.freq = min_freq, max.words = max_words, random.order = FALSE,
                      colors = cols(cols_n))
            dev.off()
      }else{
            wordcloud(tweetscorpus, min.freq = min_freq, max.words = max_words, random.order = FALSE,
                      colors = cols(cols_n), rot.per = 0.3)
      }
      
      #Make a wordcloud
      wordcloud(tweetscorpus, min.freq = min_freq, max.words = max_words, random.order = FALSE,
                colors = cols(cols_n), use.r.layout = FALSE)
      
      #Make a TDM
      tweetstdm <- TermDocumentMatrix(tweetscorpus)
      tdm_freq <- as.numeric(readline("Enter lower frequency limit for tdm: "))
      
      #Find most frequent terms in TDM
      tdm <- findFreqTerms(tweetstdm, lowfreq = tdm_freq)
      tdm
      
      #Ask user for terms for correlation analysis
      assoc_term <- readline("Enter association term(s) separated with commas: ")
      assoc_term <- strsplit(assoc_term, split = ", ")
      assoc_vector <- c()
      assoc_matrix <- sapply(assoc_term, function(x) x)
      if(length(assoc_matrix) == 1){
                  assoc_vector[1] <- assoc_matrix[1]
      }else{
            for(i in 1:nrow(assoc_matrix)){
                  assoc_vector[i] <- assoc_matrix[i,1]
            }
      }
      
      assoc_corr <- as.numeric(readline("Enter lower correlation limit: "))
      findAssocs(tweetstdm, terms = assoc_vector, corlimit = assoc_corr)
}