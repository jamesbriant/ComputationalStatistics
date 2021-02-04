################
# Question 1
# Part b)

f <- function(x, d){
  a <- 1 + (x^2)/d
  b <- (d+1)/2
  
  return(a^(-b))
}

d <- 10

a <- 1
b1 <- -sqrt((2*d)/(d-1))
b2 <- sqrt((2*d)/(d-1))

N <- 10000

X <- numeric(N)
tries <- numeric(N)

i <- 1
j <- 0
while(i <= N){
  j <- j + 1  
  
  U <- runif(1, 0, a)
  U2 <- runif(1, 0, 1)
  V <- b1 + (b2 - b1)*U2
  
  if(U^2 < f(V/U, d)){
    X[i] <- V/U
    tries[i] <- j
    
    i <- i + 1
    j <- 0
  }
}

mean(X)
var(X)

hist(X, freq=FALSE)
x <- seq(-6, 6, length=300)
lines(x, dt(x, d), col="red")

hist(tries, freq=F)
mean(tries)













t.rou.fun = function(d){
  
  xhat = sqrt(2*d/(d-1))
  b=xhat*(1+xhat^2/d)^(-(d+1)/4)
  success=FALSE
  counter=0
  while(success==FALSE){
    counter=counter+1
    u=runif(1)
    v=runif(1,-b,b)
    x=v/u
    if(u^2 < (1+x^2/d)^(-(d+1)/2)){success=TRUE;x=v/u}
  }
  c(x,counter)  
}



xsim=numeric(10000)
Ntry=numeric(10000)
for(i in 1:10000){res=t.rou.fun(10);xsim[i]=res[1];Ntry[i]=res[2]}


hist(Ntry,freq=FALSE,main="Histogram of number of attempts",xlab="")






















