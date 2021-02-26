data <- c(0.02, -0.18, -1.37, -0.60, 0.29)
data.mean <- mean(data)
sum.data.squared <- sum(data^2)

#################

MCMC_MH1 <- function(x0, N, a, burn.in.size=100){
  MaxT <- N + burn.in.size
  samples <- numeric(MaxT+1)
  samples[1] <- x0
  
  for(t in 1:MaxT){
    x <- samples[t]
    
    y <- x + runif(1, -a, a)
    
    alpha <- dnorm(y, data.mean, 1, log=TRUE) - dnorm(x, data.mean, 1, log=TRUE)
      
    u <- runif(1, 0, 1)
    
    if(log(u) < alpha){
      samples[t+1] <- y
    }else{
      samples[t+1] <- x
    }
  }
  
  return(samples[(burn.in.size+2):(MaxT+1)])
}

MCMC.data1 <- MCMC_MH1(5, 5000, 3)
ts.plot(MCMC.data1)
mean(MCMC.data1)

acf(MCMC.data1)

######################

MCMC_MH2 <- function(x0, N, a, burn.in.size=100){
  MaxT <- N + burn.in.size
  samples <- numeric(MaxT+1)
  samples[1] <- x0
  
  for(t in 1:MaxT){
    x <- samples[t]
    
    y <- rnorm(1, -0.3, 1)
    
    alpha <- dnorm(y, data.mean, 1, log=TRUE) - dnorm(x, data.mean, 1, log=TRUE)
    
    u <- runif(1, 0, 1)
    
    if(log(u) < alpha){
      samples[t+1] <- y
    }else{
      samples[t+1] <- x
    }
  }
  
  return(samples[(burn.in.size+2):(MaxT+1)])
}

MCMC.data2 <- MCMC_MH2(5, 5000, 3)
ts.plot(MCMC.data2)
mean(MCMC.data2)

acf(MCMC.data2)

######################################################################
# Question 2

pi.dist.log <- function(theta, phi){
  a <- 10*log(phi)
  b <- -(1+5*phi)*theta^2
  c <- 40*theta*phi-81*phi
  return(a+b+c)
}

MCMC_MH3 <- function(x0, N=5000, variances=c(1,1), burn.in.size=100){
  MaxT              <- N + burn.in.size
  accept.counter    <- 0
  
  samples.theta     <- numeric(MaxT+1)
  samples.phi       <- numeric(MaxT+1)
  samples.theta[1]  <- x0[1]
  samples.phi[1]    <- x0[2]
  
  for(t in 1:MaxT){
    x.theta <- samples.theta[t]
    x.phi   <- samples.phi[t]
    
    y.theta <- rnorm(1, x.theta, sd=sqrt(variances[1]))
    y.phi   <- rnorm(1, x.phi, sd=sqrt(variances[2]))
    
    ## check if phi is negative and skip if so
    if(y.phi < 0){
      samples.theta[t+1]  <- x.theta
      samples.phi[t+1]    <- x.phi
      next
    }
    
    alpha   <- pi.dist.log(y.theta, y.phi) - pi.dist.log(x.theta, x.phi)
    
    u       <- runif(1, 0, 1)
    
    if(log(u) < alpha){
      if(t > burn.in.size){
        accept.counter <- accept.counter + 1
      }
      samples.theta[t+1]  <- y.theta
      samples.phi[t+1]    <- y.phi
    }else{
      samples.theta[t+1]  <- x.theta
      samples.phi[t+1]    <- x.phi
    }
  }
  
  return(list("theta"=samples.theta[(burn.in.size+2):(MaxT+1)],
              "phi"=samples.phi[(burn.in.size+2):(MaxT+1)],
              "prob.acc"= accept.counter/N)
         )
}


MCMC.data3 <- MCMC_MH3(x0=c(4,10), variances=c(0.03,9))
plot(MCMC.data3$theta, MCMC.data3$phi)
MCMC.data3$prob.acc

ts.plot(MCMC.data3$theta)
ts.plot(MCMC.data3$phi)

##################

mean(MCMC.data3$theta*MCMC.data3$phi)
mean(MCMC.data3$theta)
sum(MCMC.data3$phi > 10)/5000
