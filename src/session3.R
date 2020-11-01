#############################################################
# Week 4 - Session 3


loglike <- function(alpha, beta, x){
  # x should be a vector!!
  n <- length(x)
  
  a <- n*log(beta)
  b <- sum(log(rep(beta^2, n) + (x-alpha)^2))
  
  return(a-b)
}

loglikeFirstDerivativeAlpha <- function(alpha, beta, x){
  # x should be a vector!
  
  a <- 2*(x-alpha)
  b <- beta^2 + (x-alpha)^2
  
  return(sum(a/b))
}

loglikeFirstDerivativeBeta <- function(alpha, beta, x){
  # x should be a vector!
  n <- length(x)
  
  a <- (x-alpha)^2
  
  return(sum((a-beta^2)/(beta^3 + beta*a)))
}

K <- 350

data <- c(-4.2, -2.85, -2.3, -1.02, 0.7, -0.98, 2.72, 3.5)
alpha <- seq(from=-5, to=2, length.out=K)
beta <- seq(from=0.01, to=2, length.out=K)

########---------------------------------------------------------------
# Question 1

z <- matrix(0, nrow=K, ncol=K)

for (i in 1:K){
  z[i, ] <- sapply(alpha, loglike, beta[i], data)
}

persp(beta, alpha, z)
contour(alpha, beta, z)


#########-------------------------------------------------------------------
# Question 2

gradVector <- function(alpha, beta, x){
  # x should be a vector!
  return(list(u = loglikeFirstDerivativeAlpha(alpha, beta, x), 
              v = loglikeFirstDerivativeBeta(alpha, beta, x)))
}


#######-------------------------------------------------------------------
# Question 3

N <- 15

alpha0 <- 0.5
beta0 <- 0.5

alpha_estimates <- numeric()
alpha_estimates[1] <- alpha0
beta_estimates <- numeric()
beta_estimates[1] <- beta0

uv <- gradVector(alpha_estimates[1], beta_estimates[1], data)
alpha_estimates[2] <- alpha_estimates[1] + delta * uv$u
beta_estimates[2] <- beta_estimates[1] + delta*uv$v

TOL <- 1e-4
delta <- 0.1
i <- 2

while(abs(loglike(alpha_estimates[i], beta_estimates[i], data) - loglike(alpha_estimates[i-1], beta_estimates[i-1], data)) > TOL){
  uv <- gradVector(alpha_estimates[i], beta_estimates[i], data)
  alpha_estimates[i+1] <- alpha_estimates[i] + delta * uv$u
  beta_estimates[i+1] <- beta_estimates[i] + delta*uv$v
  
  if(loglike(alpha_estimates[i+1], beta_estimates[i+1], data) < loglike(alpha_estimates[i], beta_estimates[i], data)){
    delta <- delta/2
  }
  
  i <- i + 1
}


#########------------------------------------------------------------------
# Question 4

contour(alpha_estimates, beta_estimates, z)
lines(mapply(loglike, alpha_estimates, beta_estimates, rep(list(data), length(alpha_estimates))))







