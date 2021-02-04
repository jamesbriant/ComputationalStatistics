############-----------------------------------------------------------
# Question 1

f.prop <- function(x){
  return(x^2 * exp(-(x^2)/2 - 2*x))
}

N <- 1000
n <- 3
gamma <- 2

M <- 1

X <- numeric(N)
i <- 1
while(i <= 1000){
  Y <- rgamma(1, 3, rate=2)
  U <- runif(1, 0, 1)
  
  if(U * M * dgamma(Y, 3, rate=2) < f.prop(Y)){
    X[i] <- Y
    i <- i + 1
  }
}

mean(X)
var(X)



############-----------------------------------------------------------
# Question 2

data <- c(0.02, -0.18, -1.37, -0.6, 0.29)

f.prop <- function(theta){
  data <- c(0.02, -0.18, -1.37, -0.6, 0.29)
  return(exp(-0.5*sum((data - theta)^2))/(1 + theta^2))
}

N <- 1000

M <- exp(0.5*sum((data - mean(data))^2))

X <- numeric(N)
i <- 1
while(i <= 1000){
  Y <- rcauchy(1, 0, 1)
  U <- runif(1, 0, 1)
  
  if(U * M * dcauchy(Y, 0, 1)< f.prop(Y)){
    X[i] <- Y
    i <- i + 1
  }
}

mean(X)
length(X[X>0])/N

hist(X, freq=FALSE)
lines(seq(-2, 1, length=300), sapply(seq(-2, 1, length=300), f.prop)/0.39, col="red")






























