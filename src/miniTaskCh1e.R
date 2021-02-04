# Question 1
# a)

y.hat <- 3^0.25

H <- function(y){
  return((6*y - 10*y^5 + y^6)*exp(-(y^4)/4)) 
}

f <- function(y){
  return(((y^3)*exp(-(y^4)/4)))
}

y <- 1.8

integral = (f(y.hat)*(2*pi)^0.5/abs(H(y.hat))^0.25)*(pnorm((y-y.hat)*sqrt(abs(H(y.hat)))) - pnorm((0-y.hat)*sqrt(abs(H(y.hat)))))
integral


































