############-----------------------------------------------------------
# Question 1
# Part a)

f <- function(x){
  return((x^2 * exp(-(x^2)/2 - 2*x))/0.1068)
}

N <- 1000

M <- gamma(3)/(0.1068*(2^3))

X <- numeric(N)
tries <- numeric(N)

i <- 1
j <- 0
while(i <= 1000){
  j <- j + 1  
  Y <- rgamma(1, 3, rate=2)
  U <- runif(1, 0, 1)
  
  if(U * M * dgamma(Y, 3, rate=2) < f(Y)){
    X[i] <- Y
    tries[i] <- j
    
    i <- i + 1
    j <- 0
  }
}

mean(X)
var(X)

hist(tries, freq=FALSE)
mean(tries)
M



############
# Part d&e)

x <- seq(0, 4, length=300)
plot(x, f(x), type="l", ylim=c(0, 1.3))
lines(x, M*dgamma(x, 3, 2), col="red")

M.full <- (2*exp(0.5))/(3^3 * 0.1068)
lines(x, M.full*dgamma(x, 3, 3), col="blue")






