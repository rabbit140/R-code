nicepie <- function(x, main = "title", r1 = 0.7, r2 = r1/2, init.angle = 30, ...){
      args1 <- list(init.angle)
      inargs <- list(...)
      args1[names(inargs)] <- inargs
      suma <- sum(x)
      procenty <- unname(x)/suma*100
      do.call(pie, c(list(x=x), args1))
      pie(x, main = main, init.angle = init.angle, 
          labels = paste(names(x), paste(round(procenty), "%", sep = "")), radius = r1)
}