######################
# LIBRARIES
#####################

library(mvtnorm)

#####################
# CODE
####################

GibbsSampler <- function(N, x0, y0, rho=0.3){
  x <- numeric(N+1)
  y <- numeric(N+1)
  
  x[1] <- x0
  y[1] <- y0
  
  sigma <- 1-rho^2
  
  for(i in 1:N){
    x[i+1] <- rnorm(1, rho*y[i], sigma)
    y[i+1] <- rnorm(1, rho*x[i+1], sigma)
  }
  
  return(list("x"=x[2:(N+1)],
              "y"=y[2:(N+1)])
         )
}

N <- 500
rho <- 0.3

data <- GibbsSampler(N, 0, 0, rho)
x <- data$x
y <- data$y

ts.plot(x)
ts.plot(y)

hist(x)
hist(y)

acf(x)
acf(y)

##############################################
# Part d)

M <- 500
x.plot <- seq(-5, 5, length=M)
y.plot <- seq(-5, 5, length=M)

z <- matrix(0, nrow=M, ncol=M)

pb <- txtProgressBar(min = 0, max = M, style = 3)
for(i in 1:M){
  for(j in 1:M){
    z[i, j] <- dmvnorm(c(x.plot[i], y.plot[j]), mean = c(0,0), sigma=matrix(c(1, rho, rho, 1), nrow=2, byrow=TRUE))
  }
  
  setTxtProgressBar(pb, i)
}

contour(x.plot, y.plot, z)
points(x, y)

##########################################
# Part e)

rho <- 0.9

data <- GibbsSampler(N, 0, 0, rho)
x <- data$x
y <- data$y

ts.plot(x)
ts.plot(y)

hist(x)
hist(y)

acf(x)
acf(y)



M <- 500
x.plot <- seq(-5, 5, length=M)
y.plot <- seq(-5, 5, length=M)

z <- matrix(0, nrow=M, ncol=M)

pb <- txtProgressBar(min = 0, max = M, style = 3)
for(i in 1:M){
  for(j in 1:M){
    z[i, j] <- dmvnorm(c(x.plot[i], y.plot[j]), mean = c(0,0), sigma=matrix(c(1, rho, rho, 1), nrow=2, byrow=TRUE))
  }
  
  setTxtProgressBar(pb, i)
}

contour(x.plot, y.plot, z)
points(x, y)












