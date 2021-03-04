GibbsSampler <- function(N, x0, p0, alpha=4, beta=2){
  x <- numeric(N+1)
  p <- numeric(N+1)
  
  x[1] <- x0
  p[1] <- p0
  
  for(i in 1:N){
    x[i+1] <- rbinom(1, 10, p[i])
    p[i+1] <- rbeta(1, x[i+1] + alpha, 10 - x[i+1] + beta)
  }
  
  return(list("x"=x[2:(N+1)],
              "p"=p[2:(N+1)])
  )
}



N <- 500
alpha <- 4
beta <- 2


data <- GibbsSampler(N, 5, 0.5, alpha, beta)
x <- data$x
p <- data$p

ts.plot(x)
ts.plot(p)

hist(x)
hist(p)

acf(x)
acf(p)

#################################################################
# Question 2


GibbsSampler <- function(N, x0, y0, rho=0.3){
  x <- numeric(N+1)
  y <- numeric(N+1)
  
  x[1] <- x0
  y[1] <- y0
  
  sigma <- 1-rho^2
  
  for(i in 1:N){
    x[i+1] <- rnorm(1, 0, 2+2*rho)
    y[i+1] <- rnorm(1, 0, 2-2*rho)
  }
  
  return(list("x"=x[2:(N+1)],
              "y"=y[2:(N+1)])
  )
}

N <- 500
rho <- 0.9

data <- GibbsSampler(N, 0, 0, rho)
u <- data$x
v <- data$y

x <- (u+v)/2
y <- (u-v)/2

ts.plot(x)
ts.plot(y)

hist(x)
hist(y)

acf(x)
acf(y)














