ApproximateIntegral <- function(n=10){
  X <- rgamma(n, 0.5, rate=2)
  return(sum(X)*gamma(0.5)/(sqrt(2)*n))
}

CollectApproximations <- function(n, N){
  return(sapply(rep(n, N), ApproximateIntegral))
}

data <- CollectApproximations(10, 1000)
hist(data)
mean(data)
var(data)

data2 <- CollectApproximations(100, 1000)
hist(data2)
mean(data2)
var(data2)

true.mean <- gamma(1.5)/(2^1.5)
true.mean

##################################################################
# Question 2

n <- 10000
X.values <- numeric(n)
Y.values <- numeric(n)

for(i in 1:n){
  Y.values[i] <- rgamma(1.5, 2)
  X.values[i] <- rnorm(1, 0, 1/Y.values[i])
}

E.x2y <- mean(X.values^2 * Y.values)
E.x2y

E.x <- mean(X.values)
E.x