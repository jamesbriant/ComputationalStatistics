# Question 2
# b)

SimulateWeibull <- function(N, alpha, beta){
  U <- runif(N, 0, 1)
  a <- -log(U)
  return(beta*a^(1/alpha))
}

TrueWeibull <- function(x, alpha, beta){
  a <- alpha/beta
  b <- x/beta
  c <- b^(alpha-1)
  d <- b^alpha
  
  return(a*c*exp(-d))
}

data <- SimulateWeibull(1000, 2, 3)
hist(data, freq=FALSE)

x <- seq(0, 8, length=1000)
lines(x, TrueWeibull(x, 2, 3))

# E[X}
mean(data)
# P(X>4)
sum(data>4)/length(data)


############################################################################
# Question 3
# a)

SimulateBeta <- function(n, p, q){
  x1 <- rgamma(n, p, 1)
  x2 <- rgamma(n, q, 1)
  
  return(x1/(x1+x2))
}

hist(SimulateBeta(1000, 2, 3), freq=FALSE)

# b)

SimulateParticle <- function(){
  x <- numeric(10)
  y <- numeric(10)
  
  W <- SimulateBeta(1, 2, 3)
  theta <- 2*pi*W
  R <- SimulateWeibull(1, 2, 2+sin(theta))
  
  x[1] <- R*cos(theta)
  y[1] <- R*sin(theta)
  
  for(i in 2:10){
    W <- SimulateBeta(1, 2, 3)
    theta <- 2*pi*W
    R <- SimulateWeibull(1, 2, 2+sin(theta))
    
    x[i] <- x[i-1] + R*cos(theta)
    y[i] <- y[i-1] + R*sin(theta)
  }
  
  return(list(x=x, y=y))
}

data <- SimulateParticle()
plot(c(0, data$x), c(0, data$y), type="l")

# c)

N <- 1000
x.final <- numeric(N)
y.final <- numeric(N)

for(i in 1:N){
  data <- SimulateParticle()
  x.final[i] <- data$x[10]
  y.final[i] <- data$y[10]
}

plot(x.final, y.final)






















