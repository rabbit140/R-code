knn_plot <- function(k_max = 1, seed = 1, set_seed = TRUE){
      
      #Load necessary stuff
      
      library(ggplot2)
      library(ISLR)
      library(MASS)
      library(class)
      data(Smarket)
      train = (Smarket$Year < 2005)
      Smarket.2005 = Smarket[!train,]
      Direction.2005 = Smarket$Direction[!train]
      train.X = cbind(Smarket$Lag1, Smarket$Lag2)[train,]
      test.X = cbind(Smarket$Lag1, Smarket$Lag2)[!train,]
      train.Direction = Smarket$Direction[train]
      
      #Create vectors
      k_vec <- seq(1:k_max)
      model_acc <- numeric(length(k_vec))
      
      #Use seed?
      if(set_seed == TRUE){
            set.seed(seed)
            
            #Main for loop (seed)
            for(i in k_vec){
                  knn.pred = knn(train.X, test.X, train.Direction, k = i)
                  model_acc[i] <- mean(knn.pred == Direction.2005)
            }
      }else{
            #Main for loop (no seed)
            for(i in k_vec){
                  knn.pred = knn(train.X, test.X, train.Direction, k = i)
                  model_acc[i] <- mean(knn.pred == Direction.2005)
      }}
      

      #Create df
      model_df <- data.frame(k = k_vec, acc = model_acc)
      
      #Print the parameter giving the highest accuracy
      max_acc_idx <- which(model_df$acc == max(model_df$acc))
      max_acc_df <- model_df[max_acc_idx,]
      
      #Plot the results
      g <- ggplot(model_df, aes(x = k, y = acc))
      model_plot <- g + 
            geom_line() +
            geom_smooth(method = "loess", lwd = 1.3, color = "blue") +
            geom_point(data = max_acc_df, aes(x = k, y = acc), color = "red", size = 4.5) +
            geom_text(aes(label = ifelse(acc == max(acc), paste("k =", k), "" )), hjust = -0.1, vjust = -0.7) +
            geom_vline(xintercept = max_acc_df$k, lty = 2, lwd = 1) + 
            ylab("Model Accuracy")
            
                  
      print(model_plot)
      
      
}