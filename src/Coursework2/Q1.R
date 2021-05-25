library(foreach)
library(parallel)
library(doParallel)

MixtureDensity <- function(x, mu0=0, mu1=3, p=0.3){
  a <- dnorm(x, mu0)
  b <- dnorm(x, mu1)
  
  return((1-p)*a + p*b)
}

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

theta.true <- MixtureDensity(0)
h <- c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8) # c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
h.counts <- numeric(length(h))

N <- 1000 # number of CI to generate
B <- 1000 # try for 1000, 3000, 5000, 10000? # Number of bootstrap estimates per CI

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
    
    # Is theta.true in the CI
    if(lower < theta.true && upper > theta.true){
      h.count <- h.count + 1
    }
  }
  
  h.count
}
parallel::stopCluster(cl)


print(h.counts/N)





##############################################################
#
theta.true <- MixtureDensity(0)
h <- c(0.6, 0.61, 0.62, 0.63, 0.64, 0.65, 0.66, 0.67, 0.68, 0.69, 0.7) # c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
h.counts <- numeric(length(h))

N <- 1000 # number of CI to generate
B <- 1000 # try for 1000, 3000, 5000, 10000? # Number of bootstrap estimates per CI

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
    
    # Is theta.true in the CI
    if(lower < theta.true && upper > theta.true){
      h.count <- h.count + 1
    }
  }
  
  h.count
}
parallel::stopCluster(cl)


print(h.counts/N)



