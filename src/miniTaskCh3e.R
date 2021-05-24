##################
# part a)

y.obs <- c(0.02, 0.05, 0.13, 0.14, 0.14, 0.35, 0.36, 0.44, 0.47, 0.49)
y.obs.sum <- sum(y.obs)

t <- 0.5

N <- 4000
lambdas <- numeric(N+1)
y.censored <- matrix(0, nrow=N+1, ncol=10)

alpha <- 0.01
beta <- 0.01

lambdas[1] <- 1.3
y.censored[1, ] <- t + rexp(10, rate=lambdas[1])

for(i in 2:(N+1)){
  lambdas[i] <- rgamma(1, 20 + alpha, beta + sum(y.censored[i-1, ]) + y.obs.sum)
  
  y.censored[i, ] <- t + rexp(10, rate=lambdas[i])
}

ts.plot(lambdas)
ts.plot(y.censored[, 3])

mean(lambdas)
colMeans(y.censored)

##################
# part b)

hist(lambdas)









