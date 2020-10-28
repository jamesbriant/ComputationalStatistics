#############
# a)

EstimateNormal <- function(a, b, n){
  h <- (b-a)/n
  
  x <- a + (1:n - 0.5)*h
  return(h*sum(dnorm(x)))
}

EstimateNormal(-1, 2, 9)

###########----------------------------------------------------------
# b)

n <- 7:50
integralEstimates <- mapply(EstimateNormal, -1, 2, n)
errors <- pnorm(2) - pnorm(-1) - integralEstimates

plot(n, abs(errors))


