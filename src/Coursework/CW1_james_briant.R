###########-------------QUESTION 1a--------------------------------------
# This section contains only code for use in part b.

y <- scan("Q1Data.txt")
t <- length(y) # = 50

mu0 <- 20 # given in question

f <- function(data, mu){
  # returns the square error
  return(sum((data - mu)^2))
}

LogisticMap <- function(x, r, K){
  # Calculates the next time step using the logistic map
  return(r*x*(1-x/K))
}

CalculateMu <- function(r, K, mu0, maxT){
  # Runs the dynamic model for given r, K and mu0 up to time maxT
  # Returns a vector of mu values for times {1, ..., maxT}
  mu <- numeric(maxT)
  
  mu[1] <- LogisticMap(mu0, r, K)
  for(i in 2:maxT){
    mu[i] <- LogisticMap(mu[i-1], r, K)  
  }
  
  return(mu)
}

GoldenRatioPoints <- function(x1, x3){
  # Calculate the optimum points for the Golden Ratio Mean Search Algorithm
  goldenRatio <- (1+sqrt(5))/2
  r <- 1/goldenRatio
  
  x2 <- r*x1 + (1-r)*x3
  x4 <- (1-r)*x1 + r*x3
  
  return(list("x2"=x2, "x4"=x4))
}

MinimiseF.fix.K <- function(data, mu0, r0, r1, K){
  # Run the Golden Ratio Search algorithm searching over r with a fixed K
  # returns a range in which the minimum exists
  
  x1 <- r0
  x3 <- r1
  
  t <- length(data) # number of time steps
  
  # The golden ratio algorithm
  while(abs(x3-x1) > 0.001){
    GR <- GoldenRatioPoints(x1, x3)
    x2 <- GR$x2
    x4 <- GR$x4
    
    # f is the objective function
    fx2 <- f(data, CalculateMu(x2, K, mu0, t))
    fx4 <- f(data, CalculateMu(x4, K, mu0, t))
    
    if(fx4 < fx2){
      x1 <- x2
    } else{
      x3 <- x4
    }
  }
  
  return(c(x1, x3))
}

MinimiseF.fix.r <- function(data, mu0, r, K0, K1){
  # Run the Golden Ratio Search algorithm searching over K with a fixed r
  # returns a range in which the minimum exists
  
  x1 <- K0
  x3 <- K1
  
  t <- length(data) # number of time steps
  
  # The golden ratio algorithm
  while(abs(x3-x1) > 0.001){
    GR <- GoldenRatioPoints(x1, x3)
    x2 <- GR$x2
    x4 <- GR$x4
    
    # f is the objective function
    fx2 <- f(data, CalculateMu(r, x2, mu0, t))
    fx4 <- f(data, CalculateMu(r, x4, mu0, t))
    
    if(fx4 < fx2){
      x1 <- x2
    } else{
      x3 <- x4
    }
  }
  
  return(c(x1, x3))
}



#########-----------QUESTION 1B------------------------------------------

# Figure 1 --------------------
plot(1:t, y, main="Trace of Observations", type="l", xlab="time", ylab="Observation")
abline(h=mean(y), col="red")


# Figure 2--------------------
hist(y, freq=FALSE, main="Histogram of the Data")
x <- seq(22, 28, length=200)
lines(x, dnorm(x, mean(y), 1), col="red")


# Figure 3---------------------
y.bar <- mean(y)
r <- seq(-3, 5, length=400)
K <- numeric(length(r))
z <- numeric(length(r))
for(i in 1:length(r)){
  K[i] <- -y.bar*r[i]/(1-r[i])
  z[i]<- f(y, CalculateMu(r[i], K[i], mu0, t)) # objective function
}

plot(r, K, type="l", main="K Against r")
abline(v=1, col="red")


# Figure 4---------------------
plot(r, z, type="l", main="Least Squares Objective against r", ylab="f(r, K)")


# Figure 5-------------------
r <- seq(1.1, 3, length=400)
K <- numeric(length(r))
z <- numeric(length(r))
for(i in 1:length(r)){
  K[i] <- -y.bar*r[i]/(1-r[i])
  z[i]<- f(y, CalculateMu(r[i], K[i], mu0, t)) # objective function
}
plot(r, z, type="l", main="Least Squares Objective against r", ylab="f(r, K)")


####### Implement the golden ratio search algorithm------------------------
FindMidpoint <- function(x, y){
  return((x+y)/2)
}

# intervals for where r and K lie
r <- c(1.1, 3)
K <- c(25, 300)

# best first guess of r and K, estimates come from 
r.hat <- 1.96
K.hat <- 49.4

# Since K is large, changes in K are easier to measure.
# Hence use K to stop algorithm when K converges (stops changing).
K.previous <- 0

# implement the one-dimensional search algorithm
counter <- 0
while(abs(K.previous - K.hat) > 0.000000001 && counter < 1000){
  counter <- counter + 1
  
  r.range <- MinimiseF.fix.K(y, mu0, r[1], r[2], K.hat) # search over all possible r
  r.hat <- FindMidpoint(r.range[1], r.range[2])
  
  K.range <- MinimiseF.fix.r(y, mu0, r.hat, K[1], K[2]) # search over all possible K
  K.previous <- K.hat
  K.hat <- FindMidpoint(K.range[1], K.range[2])
}
r.hat
K.hat
f(y, CalculateMu(r.hat, K.hat, mu0, t))
K.hat * (r.hat-1)/r.hat


# Figure 6-------------
r <- seq(1.8, 2.2, length=40)
K <- seq(40, 60, length=40)
z <- matrix(0, nrow=length(r), ncol=length(K))
for(i in 1:length(r)){
  for(j in 1:length(K)){
    z[i, j] <- f(y, CalculateMu(r[i], K[j], mu0, t))
  }
}

persp(K, r, t(z), zlab="f")


# Figure 7-----------------
contour(r, K, z, xlab="r", ylab="K")
lines(r, K, col="red")
points(r.hat, K.hat, col="blue")






###########-------------QUESTION 2a--------------------------------------


###########-------------QUESTION 2b--------------------------------------
# defines useful functions from handwritten work in 2a)

beta.hat <- function(gamma){
  # Find the optimum beta given gamma
  return((gamma + sqrt(gamma^2 + 12))/2)
}

M <- function(gamma, k){
  # Return M as a function of gamma
  # k is the normalising constant
  beta <- beta.hat(gamma)
  
  a <- 2/(beta^3*k)
  b <- (beta-gamma)^2/2
  
  return(a*exp(b))
}

M(2, 0.1068461)
M(-5, 17488148)


###########-------------QUESTION 2c--------------------------------------
# defines useful functions from handwritten work

y.hat <- function(gamma){
  a <- -gamma + sqrt(gamma^2 + 8)
  return(a/2)
}

H <- function(gamma){
  a <- y.hat(gamma)^2
  return((a + 2)/a)
}

x.hat <- function(gamma, lambda){
  A <- ((H(gamma)/lambda)-1)/2
  B <- -gamma - H(gamma)/lambda*y.hat(gamma)
  
  return((-B - sqrt(B^2 - 16*A))/(4*A))
}

M1 <- function(gamma, lambda){
  # Return M1 as a function of gamma
  x <- x.hat(gamma, lambda)
  H.gamma <- H(gamma)/lambda
  
  a <- x^2*(H.gamma-1)/2
  b <- -x*(gamma + H.gamma*y.hat(gamma))
  c <- H.gamma * y.hat(gamma)^2/2
  
  return(x^2 * exp(a + b + c))
}


###########-------------QUESTION 2d--------------------------------------

f1 <- function(x, gamma){
  # function f up to proportionality
  x2 <- x^2
  a <- x2/2 + gamma*x
  return(x2*exp(-a))
}

# Figure 8-----------
x <- seq(0, 4, length=500)
plot(x, f1(x,2)/0.1068461, type="l", main="gamma = 2", ylab="density")
lines(x, dnorm(x, y.hat(2), 5/H(2)), col="red")


# Figure 9-----------
x <- seq(0, 11, length=500)
plot(x, f1(x,-5)/17488148, type="l", main="gamma = -5", ylab="density")
lines(x, dnorm(x, y.hat(-5), 1.5/H(-5)), col="red")


###########-------------QUESTION 2e--------------------------------------
#### Rejection algorithm implementation ####

N <- 100000 # number of samples to be drawn

mu <- y.hat(-5)
sigma2 <- 1.5/H(-5)

X <- numeric(N)
tries <- numeric(N) # counts the attempts

i <- 1
j <- 0
while(i <= N){
  j <- j + 1  
  Y <- rnorm(1, mu, sqrt(sigma2))
  U <- runif(1, 0, 1)
  
  if(U * M1(-5, 1.5) * dnorm(Y, mu, sqrt(sigma2)) * sqrt(2*pi*sigma2) < f1(Y, -5)){
    X[i] <- Y
    tries[i] <- j
    
    i <- i + 1 # move on to next sample
    j <- 0 # reset attempts counter
  }
}


# Figure 10---------------
hist(X, freq=FALSE, breaks=seq(min(X), max(X), length=30), main="Distribution of f")
lines(seq(min(X), max(X), length=400), f1(seq(min(X), max(X), length=400), -5)/17488148, col="red")


# Figure 11------------
hist(tries, freq=FALSE, main="Histrogram of Number of Rejection Algorithm Attemps")



###########-------------QUESTION 2f--------------------------------------




###########-------------QUESTION 2g--------------------------------------




