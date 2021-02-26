############### MCMC - Metropolis-Hastings implementation

burn.in.size <- 100
MaxT <- 5000 + burn.in.size
samples <- numeric(MaxT+1)
samples[1] <- 5
acceptance.counter <- 0
sigma <- 0.2

for(t in 1:MaxT){
  # previous value
  x <- samples[t]
  
  # new proposed value
  y <- rnorm(1, mean=x, sd=sigma)
  
  # log probability
  alpha <- dnorm(y, mean=0, sd=sigma, log=TRUE) - dnorm(x, mean=0, sd=sigma, log=TRUE)
  
  u <- runif(1, 0, 1)
  
  if(log(u) < alpha){
    if(t > burn.in.size){
      acceptance.counter <- acceptance.counter + 1
    }
    samples[t+1] <- y
  } else{
    samples[t+1] <- x
  }
}


samples.adjusted <- samples[(burn.in.size+2):(MaxT+1)]
ts.plot(samples.adjusted)
acceptance.counter/(MaxT-burn.in.size)
mean(samples.adjusted)

######################################
hist(samples.adjusted, freq=FALSE)
x <- seq(-1, 1, length=500)
lines(x, dnorm(x, 0, sigma), col="red")
