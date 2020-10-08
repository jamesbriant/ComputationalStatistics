#############################################################
# Week 3 - Session 2

loglike <- function(alpha, beta, x){
  # x should be a vector!!
  n <- length(x)
  
  a <- n*log(beta/pi)
  b <- sum(log(beta^2 + (x-alpha)^2))
  
  return(a-b)
}

loglikeFirstDerivative <- function(alpha, beta, x){
  # x should be a vector!
  n <- length(x)
  
  a <- 2*sum(x-alpha)
  b <- n*(beta^2) + sum((x-alpha)^2)
  
  return(a/b)
}

loglikeSecondDerivative <- function(alpha, beta, x){
  # x should be a vector!
  n <- length(x)
  
  b2 <- beta^2
  b4 <- b2^2
  a2 <- sum((x-alpha)^2)
  a4 <- sum((x-alpha)^4)
  
  numerator <- -2*n*b2 + 2*a2
  denominator <- n*b4 + 2*b2*a2 + a4
  
  return(numerator/denominator)
}

K <- 700

data <- c(-4.2, -2.85, -2.3, -1.02, 0.7, -0.98, 2.72, 3.5)
beta <- 0.1
alpha <- seq(from=-5, to=3, length.out=K)

plot(alpha, mapply(loglike, alpha, beta, rep(list(data), K)))
abline(v=-0.554, col="red")


#############-------------------------------------------------------
# Solve the optimisation problem using the Halving (Interval Bisection) algorithm

# This method probably doesn't work well if there are many local optima.

x1 <- -1.5
x3 <- 0

counter <- 0

while(x3-x1 > 0.001){
  x2 <- 0.5*(x1+x3)
  
  if(loglikeFirstDerivative(x2, 0.1, data) > 0){
    x1 <- x2
  } else {
    x3 <- x2
  }
  
  counter <- counter + 1
}

print(paste("(", x1, ",", x3, ") in", counter, "iterations."))


#############-------------------------------------------------------
# Solve the optimisation problem using the Golden Ratio Mean Search algorithm

goldenRatioMeanSearch <- function(x1, x3){
  
  goldenRatio <- (1+sqrt(5))/2
  r <- 1/goldenRatio
  
  x2 <- r*x1 + (1-r)*x3
  x4 <- (1-r)*x1 + r*x3
  
  return(list("x2"=x2, "x4"=x4))
}




x1 <- -3
x3 <- 1

counter <- 0

while(x3-x2 > 0.001){
  GR <- goldenRatioMeanSearch(x1, x3)
  x2 <- GR$x2
  x4 <- GR$x4
  
  fx2 <- -loglike(x2, 0.1, data)
  fx4 <- -loglike(x4, 0.1, data)
  
  if(fx4 < fx2){
    x1 <- x2
  } else{
    x3 <- x4
  }
  
  counter <- counter + 1
}

print(paste("(", x1, ",", x3, ") in", counter, "iterations."))


#############-------------------------------------------------------
# Solve the optimisation problem using the Newton-Raphson method

N <- 20

x <- numeric(N+1)
x[1] <- -0.5

for(i in 1:N){
  a <- loglikeFirstDerivative(x[i], 0.1, data)
  b <- loglikeSecondDerivative(x[i], 0.1, data)
  
  x[i+1] <- x[i] - a/b
}

print(paste("The best estimate for x is", x[N+1], "after", N, "iterations."))



