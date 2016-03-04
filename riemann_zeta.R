zeta <- function(n = 100, s = readline("Enter the argument 's': ")){
      library(ggplot2)
      s <- as.numeric(s)
      sum <- c(0)
      y <- c()
      for (i in 1:n){
            x <- 1:i
            sum <- sum + 1/(i ^ s)
            y[i] <- sum
      }
      xy_df <- data.frame(x,y)
      g <- ggplot(xy_df, aes(x, y))
      zeta <- g + geom_line() + ggtitle(paste("Plot of Riemann zeta function for n =", n, "and s =", s)) +
            xlab("n")
      print(paste("Sum:",sum))
      print(zeta)
}