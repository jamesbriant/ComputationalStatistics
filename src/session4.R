X<-read.table("data/Session4Data.txt",header=TRUE)
x<-X$x   ##### Protein measure
z<-X$z   ##### Indicator

################
# a)

hist(x, freq=FALSE)

###############
# b)

tol <- 1e-6
n <- length(x)

p <- numeric()
p[1] <- 0.5
mu0 <- numeric()
mu0[1] <- 6
mu1 <- numeric()
mu1[1] <- 5
w <- list()
a <- dnorm(x, mu0[1], 1)
b <- dnorm(x, mu1[1], 1)
w[[1]] <- p[1]*b/((1-p[1])*a + p[1]*b)

p[2] <- sum(w[[1]])/n
mu0[2] <- sum((1-w[[1]])*x)/sum(1-w[[1]])
mu1[2] <- sum(w[[1]]*x)/sum(w[[1]])

i <- 2
while(abs(p[i]-p[i-1]) > tol && abs(mu0[i] - mu0[i-1]) > tol && abs(mu1[i] - mu1[i-1]) > tol){
  a <- dnorm(x, mu0[i], 1)
  b <- dnorm(x, mu1[i], 1)
  w[[i+1]] <- p[i]*b/((1-p[i])*a + p[i]*b)
  
  p[i+1] <- sum(w[[i+1]])/n
  mu0[i+1] <- sum((1-w[[i+1]])*x)/sum(1-w[[i+1]])
  mu1[i+1] <- sum(w[[i+1]]*x)/sum(w[[i+1]])
  
  i <- i+1
  if(i==1000){
    break
  }
}

############
# c)

p.hat <- p[i]
mu0.hat <- mu0[i]
mu1.hat <- mu1[i]

x.plot <- seq(0, 10, length=100)

f <- (1-p.hat)*dnorm(x.plot, mu0.hat, 1) + p.hat*dnorm(x.plot, mu1.hat, 1)
lines(x.plot, f)
points(x.plot, f)

##########
# d)

classify <- numeric(100)
classify[w[[i]] < 0.5] <- 1

#########
# e)

correctCounter <- 100 - sum(z - classify)
correctCounter

###########
# f)

points(x[12], (1-p.hat)*dnorm(x[12], mu0.hat, 1) + p.hat*dnorm(x[12], mu1.hat, 1), col="red")



