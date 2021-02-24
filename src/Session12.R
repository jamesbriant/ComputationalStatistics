K <- 0.1068458

h <- function(x){
  return(x)
}

f <- function(x){
  a <- x^2/K
  b <- exp(- x^2/2 - 2*x)
  return(a*b)
}

g.gamma <- function(x){
  # a <- 16/gamma(4)*x^3
  # b <- exp(-2*x)
  # return(a*b)
  
  return(dgamma(x, 4, rate=2))
}

g.normal <- function(x){
  return(dnorm(x, 1, sd=sqrt(0.25)))
}

w.gamma <- function(x){
  return(f(x)/g.gamma(x))
}

w.normal <- function(x){
  return(f(x)/g.normal(x))
}

N <- 100

X.gamma <- rgamma(N, 4, rate=2)

weights.gamma <- w.gamma(X.gamma)
I.hat.gamma <- h(X.gamma)*weights.gamma
hist(I.hat.gamma)
hist(weights.gamma)

X.normal <- rnorm(N, 1, sd=sqrt(0.25))

weights.normal <- w.normal(X.normal)
I.hat.normal <- h(X.normal)*weights.normal
hist(I.hat.normal)
hist(weights.normal)

#####################
# Question 1e)

x <- seq(0, 10, length=500)
plot(x, h(x)*f(x), type="l")
lines(x, g.gamma(x), col="red")
lines(x, g.normal(x), col="blue")

####################
# Question 2b)

data <- c(0.02, -0.18, -1.37, -0.60, 0.29)
sum.data.squared <- sum(data^2)

posterior <- function(theta){
  a <- sum.data.squared - 2*theta*sum(data) + length(data)*theta^2
  
  return(exp(-0.5*a)/(1 + theta^2))
}

theta <- seq(-4, 2, length=400)
plot(theta, abs(posterior(theta)*theta), type="l")


######################
# Question 2c)

lines(theta, dnorm(theta, -0.7, 0.5), col="red")
abline(v=-0.7, col="red")

######################
# Question 2d)

h <- function(theta){
  return(theta)
}

f <- function(theta){
  data <- c(0.02, -0.18, -1.37, -0.60, 0.29)
  sum.data.squared <- sum(data^2)
  a <- sum.data.squared - 2*theta*sum(data) + length(data)*theta^2
  
  return(exp(-0.5*a)/(1 + theta^2))
}

g <- function(theta){
  return(dnorm(theta, -0.7, sd=0.5))
}

theta <- rnorm(1000, -0.7, sd=0.5)
weights <- f(theta)/g(theta)
estimate <- mean(h(theta)*weights)
estimate
