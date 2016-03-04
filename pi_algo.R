compute_pi <- function(bits = 1000000, n_iter = 10){
      
      #Load library for high-precision numbers
      library(Rmpfr)
      
      #Set initial values
      a0 = 1
      b0 = mpfr(1/sqrt(2), bits)
      t0 = 1/4
      p0 = 1
      a_t <- numeric(1)
      pi_value <- numeric(1)
      #Loop
      for(i in 0:n_iter){
            if(i == 0){
                  a = mpfr((a0+b0)/2, bits)
                  b = mpfr(sqrt(a0*b0), bits)
                  t = mpfr(t0 - p0*(a0-a)^2, bits)
                  p = mpfr(2*p0, bits)
            }
            else{
                  a = mpfr((a+b)/2, bits)
                  b = mpfr(sqrt(a_t*b), bits)
                  t = mpfr(t - p*(a_t-a)^2, bits)
                  p = mpfr(2*p, bits)
            }
            
            #Temporary value
            a_t <- a
            pi_value <- (a + b)^2/(4*t)
      }     
      pi_value
}