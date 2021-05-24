f <- function(theta, phi){
  return(phi^10*exp(-(1+5*phi)*theta^2 + 40*theta*phi - 81*phi))
}

f.log <- function(theta, phi){
  return(10*log(phi) - (1+5*phi)*theta^2 + 40*theta*phi - 81*phi)
}

# define the variance terms
sigma2.theta <- 0.3
sigma2.phi   <- 9

# number of MCMC iterations
N <- 5000

theta.estimates <- numeric(N+1)
phi.estimates   <- numeric(N+1)

# initial chain values
theta.estimates[1] <- 4
phi.estimates[1] <- 8

# generate the chain
for(i in 2:(N+1)){
  # propose new theta
  theta.prop <- rnorm(1, theta.estimates[i-1], sqrt(sigma2.theta))
  
  # calculate the log acceptance ratio
  alpha.log <- f.log(theta.prop, phi.estimates[i-1]) - 
                f.log(theta.estimates[i-1], phi.estimates[i-1]) + 
                dnorm(theta.estimates[i-1], theta.prop, sd=sqrt(sigma2.theta), log=TRUE) - 
                dnorm(theta.prop, theta.estimates[i-1], sd=sqrt(sigma2.theta), log=TRUE)
  
  # accept/reject
  if(log(runif(1, 0, 1)) < alpha.log){
    theta.estimates[i] <- theta.prop
  }else{
    theta.estimates[i] <- theta.estimates[i-1]
  }
  
  # propose new phi
  phi.prop <- rnorm(1, phi.estimates[i-1], sqrt(sigma2.phi))
  
  if(phi.prop > 0){
    # calculate the log acceptance ratio
    alpha.log <- f.log(theta.estimates[i], phi.prop) - 
                  f.log(theta.estimates[i], phi.estimates[i-1]) + 
                  dnorm(phi.estimates[i-1], phi.prop, sd=sqrt(sigma2.phi), log=TRUE) - 
                  dnorm(phi.prop, phi.estimates[i-1], sd=sqrt(sigma2.phi), log=TRUE)
  }else{
    alpha.log <- -Inf
  }
  
  # accept/reject
  if(log(runif(1, 0, 1)) < alpha.log){
    phi.estimates[i] <- phi.prop
  }else{
    phi.estimates[i] <- phi.estimates[i-1]
  }
  
}

ts.plot(theta.estimates)
ts.plot(phi.estimates)

plot(theta.estimates, phi.estimates)

#########################
# part b)


theta.estimates <- numeric(N+1)
phi.estimates   <- numeric(N+1)

# initial chain values
theta.estimates[1] <- 4
phi.estimates[1] <- 8

# generate the chain
for(i in 2:(N+1)){
  # propose new theta
  theta.prop <- rnorm(1, theta.estimates[i-1], sqrt(sigma2.theta))
  
  if(theta.prop > 0){
    # calculate the log acceptance ratio
    alpha.log <- f.log(theta.prop, phi.estimates[i-1]) - 
      f.log(theta.estimates[i-1], phi.estimates[i-1]) + 
      dnorm(theta.estimates[i-1], theta.prop, sd=sqrt(sigma2.theta), log=TRUE) - 
      dnorm(theta.prop, theta.estimates[i-1], sd=sqrt(sigma2.theta), log=TRUE)
  }else{
    alpha.log <- -Inf
  }
  
  # accept/reject
  if(log(runif(1, 0, 1)) < alpha.log){
    theta.estimates[i] <- theta.prop
  }else{
    theta.estimates[i] <- theta.estimates[i-1]
  }
  
  
  # propose new phi
  phi.estimates[i] <- rgamma(1, 11, 5*theta.estimates[i]^2 - 40*theta.estimates[i] + 81)
  
}

ts.plot(theta.estimates)
ts.plot(phi.estimates)

plot(theta.estimates, phi.estimates)

###############################################################################
# Question 2




