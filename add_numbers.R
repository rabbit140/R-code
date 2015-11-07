add_numbers <- function(num1, num2) {
      
      numbers <- c()
      
      for (i in num1:num2) {
            
            numbers <- c(numbers, i)
            
      }
      
      print(sum(numbers))
}