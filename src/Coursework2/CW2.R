# UNLESS YOU HAVE AT LEAST 6 FAST CPU CORES I DON'T RECOMMEND RUNNING PARTS 2 OR 3, 
# IT'S KINDA SLOW.

# This script 

# libraries required for parallel computations
library(foreach)
library(parallel)
library(doParallel)

# calculates true density of the mixture distribution
MixtureDensity <- function(x, mu0=0, mu1=3, p=0.3){
  a <- dnorm(x, mu0)
  b <- dnorm(x, mu1)
  
  return((1-p)*a + p*b)
}

# samples from the mixture distribution
SampleFromMixtureModel <- function(n, mu0=0, mu1=3, p=0.3){
  output.vector <- numeric(n)
  q <- 1 - p
  
  for(i in 1:n){
    if(runif(1) < q){
      output.vector[i] <- rnorm(1, mu0)
    }else{
      output.vector[i] <- rnorm(1, mu1)
    }
  }
  
  return(output.vector)
}

################################################################################
# PART 1 - Exploratory Plots

# Figure 1
x <- seq(-5, 8, length=500)
plot(x, MixtureDensity(x), type="l", main="Mixture Density", ylab="Density")
abline(v=0, col="red")

# Figure 2
data <- SampleFromMixtureModel(100)
hist(data, freq=FALSE, ylim=c(0,0.3))
leg <- c()
for(i in 3:9){
  lines(density(data, bw=0.1*i), col=i-1)
  leg <- c(leg, paste0("h=", 0.1*(i-2)))
}
lines(x, MixtureDensity(x), type="l", col=9)
legend("topright",
       legend=c("True PDF", leg),
       lty=1,
       col=1:8)
abline(v=0, col=2, lty=2)

################################################################################
# PART 2 - Generating the CIs 1

# calculate the true theta
theta.true <- MixtureDensity(0)

# value of h to explore
h <- c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8)

# vector for counting the successful CIs
h.counts <- numeric(length(h))

N <- 1000 # number of CI to generate
B <- 1000 # try for 1000, 3000, 5000, 10000? # Number of bootstrap estimates per CI

# set up the parallel environments
core.count <- detectCores()
cl <- parallel::makeCluster(core.count-1)
print(paste0("using ", core.count-1, " CPU cores."))
doParallel::registerDoParallel(cl)
h.counts <- foreach(i = 1:length(h), .combine='c', .export = ls(globalenv())) %dopar% {
  h.count <- 0
  
  for(n in 1:N){
    theta.estimates <- numeric(B)
    
    # generate the bootstrap estimates
    for(b in 1:B){
      # generate the sample
      data <- SampleFromMixtureModel(100)
      
      # calculate the bootstrap estimate
      theta.estimates[b] <- density(data, bw=h[i], from=0, to=0, n=1)$y
    }
    
    # Find the CI
    sorted.estimates <- sort(theta.estimates)
    lower <- sorted.estimates[B*0.025]
    upper <- sorted.estimates[B*0.975]
    
    # Is theta.true in the CI?
    if(lower < theta.true && upper > theta.true){
      # if yes, increase the counter
      h.count <- h.count + 1
    }
  }
  
  h.count
}
parallel::stopCluster(cl)

# print the probability approximation of the coverage
data.frame(h, h.counts/N)





################################################################################
# PART 3 - Generating the CIs 2

# Now focus on more refined area
h2 <- c(0.62, 0.63, 0.64, 0.65, 0.66, 0.67, 0.68, 0.69, 0.7, 0.71)
h2.counts <- numeric(length(h2))

N <- 1000 # number of CI to generate
B <- 1000 # try for 1000, 3000, 5000, 10000? # Number of bootstrap estimates per CI

core.count <- detectCores()
cl <- parallel::makeCluster(core.count-1)
print(paste0("using ", core.count-1, " CPU cores."))
doParallel::registerDoParallel(cl)
h2.counts <- foreach(i = 1:length(h2), .combine='c', .export = ls(globalenv())) %dopar% {
  h2.count <- 0
  
  for(n in 1:N){
    theta.estimates <- numeric(B)
    
    # generate the bootstrap estimates
    for(b in 1:B){
      # generate the sample
      data <- SampleFromMixtureModel(100)
      
      # calculate the bootstrap estimate
      theta.estimates[b] <- density(data, bw=h2[i], from=0, to=0, n=1)$y
    }
    
    # Find the CI
    sorted.estimates <- sort(theta.estimates)
    lower <- sorted.estimates[B*0.025]
    upper <- sorted.estimates[B*0.975]
    
    # Is theta.true in the CI
    if(lower < theta.true && upper > theta.true){
      h2.count <- h2.count + 1
    }
  }
  
  h2.count
}
parallel::stopCluster(cl)

# print the probability approximation of the coverage
data.frame(h2, h2.counts/N)

