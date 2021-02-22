h <- function(x){
  return(1-x^2)
}

f <- function(x){
  return(1)
}

g <- function(x){
  return(2*(1-x))
}

N <- 1000

xA <- runif(N)
mean(h(xA))

u <- runif(N)
xB <- 1-sqrt(1-u)
mean(h(xB)*f(xB)/g(xB))

##########
# Question d)

N <- 10
M <- 10000

direct.method.samples <- numeric(M)
importance.method.samples <- numeric(M)

for(i in 1:M){
  xA <- runif(N)
  direct.method.samples[i] <- mean(h(xA))
  
  u <- runif(N)
  xB <- 1-sqrt(1-u)
  importance.method.samples[i] <- mean(h(xB)*f(xB)/g(xB))
}

var(direct.method.samples)
4/(45*N)

var(importance.method.samples)
1/(72*N)






