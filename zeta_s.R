zeta_s <- function(zeta_iter = readline("Enter number of iterations for zeta function: "),
                   step = readline("Enter step size: "), 
                   s_min = readline("Enter the lower limit for s: "),
                   s_max = readline("Enter the upper limit for s: ")){
      library(dplyr)
      library(stringi)
      zeta_iter <- as.numeric(zeta_iter)
      step <- as.numeric(step)
      s_min <- as.numeric(s_min)
      s_max <- as.numeric(s_max)
      sequence <- seq(from = s_min, to = s_max, by = step)
      zeta_out <- c("")
      
      for(i in 1:length(sequence)){
            s_seq <- seq(from = 1, to = length(sequence))
            zeta_out[i] <- capture.output(zeta(n = zeta_iter, s = sequence[i]))
            zeta_out[i] <- substr(zeta_out[i], 11, stri_length(zeta_out[i]))
            zeta_out[i] <- substr(zeta_out[i], 1, stri_length(zeta_out[i]) - 1)
      }
      zeta_out <- as.numeric(zeta_out)
      summary_df <- data.frame(s = sequence, sum_func = zeta_out)
      h <- ggplot(summary_df, aes(s, sum_func))
      graph <- h + geom_line() + xlab("sum") + ggtitle(paste("Sum of Riemann's zeta for given s"))
      print(graph)
}