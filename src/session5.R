###########
# Question 1

f <- function(x, k=3, gamma=2){
  a <- x^(k-1)
  b <- -(x^2)/2 - gamma*x
  
  return(a*exp(b))
}

##########
# a)

# First check where most of the mass is to find a suitable area for
# approximating the integral

x <- seq(0, 10, length=500)
fx <- f(x)
plot(x, fx, type="l")

# Most of the mass is in [0, 4]

a <- 0
b <- 4
n <- 500
h <- (b-a)/n

x <- a + (0:n)*h
fx <- f(x)

normalisingConstant <- h*(0.5*(fx[1] + fx[n+1]) + sum(fx[2:n]))
normalisingConstant

###########
# b)

x <- a + (0:(2*n))*h/2

m1.data <- f(x, 4, 2)
m2.data <- f(x, 5, 2)

m1Estimate <- (h/6)*(m1.data[1] + m1.data[2*n+1] + 2*sum(m1.data[2*(2:n)-1]) + 4*sum(m1.data[2*(1:n)])) / normalisingConstant
m1Estimate
m2Estimate <- (h/6)*(m2.data[1] + m2.data[2*n+1] + 2*sum(m2.data[2*(2:n)-1]) + 4*sum(m2.data[2*(1:n)])) / normalisingConstant
m2Estimate

varianceEstimate <- m2Estimate - m1Estimate^2
varianceEstimate


##########------------------------------------------------------------
# Question 2
# a)

f <- function(x, y){
  a <- -x^2*y/2 - 2*y
  
  return(y*exp(a))
}

Nx <- 200
Ny <- 200

x <- seq(-15, 15, length=Nx)
y <- seq(0, 4, length=Ny)

z <- matrix(0, nrow=Nx, ncol=Ny)
for(i in 1:Nx){
  z[i, ] <- f(x[i], y)
}

persp(x, y, z)
persp(y, x, t(z))


# Most of the mass lies in -15<x<15 and 0<y<4
a <- -15
b <- 15
c <- 0
d <- 4

nx <- 300
ny <- 300

hx <- (b-a)/nx
hy <- (d-c)/ny

x <- a + (0:(2*nx))*hx/2
y <- c + (0:(2*ny))*hy/2

z <- matrix(0, nrow=length(x), ncol=length(y)) # function values
g <- numeric(length(x)) # integration wrt y

for(i in 1:length(x)){
  z[i, ] <- f(x[i], y)
  
  g[i] <- (hy/6)*(z[i, 1] + z[i, 2*ny+1] + 2*sum(z[i, 2*(2:ny)-1]) + 4*sum(z[i, 2*(1:ny)]))
}

normalisingConstant <- (hx/6)*(g[1] + g[2*nx+1] + 2*sum(g[2*(2:nx)-1]) + 4*sum(g[2*(1:nx)]))
normalisingConstant


###########
# b)

a <- -3
b <- 4
c <- 0
d <- 2

nx <- 300
ny <- 300

hx <- (b-a)/nx
hy <- (d-c)/ny

x <- a + (0:(2*nx))*hx/2
y <- c + (0:(2*ny))*hy/2

z <- matrix(0, nrow=length(x), ncol=length(y)) # function values
g <- numeric(length(x)) # integration wrt y

for(i in 1:length(x)){
  z[i, ] <- f(x[i], y)
  
  g[i] <- (hy/6)*(z[i, 1] + z[i, 2*ny+1] + 2*sum(z[i, 2*(2:ny)-1]) + 4*sum(z[i, 2*(1:ny)]))
}

prob <- (hx/6)*(g[1] + g[2*nx+1] + 2*sum(g[2*(2:nx)-1]) + 4*sum(g[2*(1:nx)])) / normalisingConstant
prob







