f <- function(x, y){
  a <- (1-x)^2
  b <- (y-x^2)^2
  return(a + 100*b)
}

fxy <- function(x, y){
  # x & y should be vectors of the same length!
  m <- length(x)
  n <- length(y)
  
  outputMatrix <- matrix(0, nrow=m, ncol=n)
  
  for(i in 1:m){
    outputMatrix[i, ] <- sapply(x[i], f, y)
  }
  
  return(outputMatrix)
}

x <- seq(from=-2, to=2, length.out=100)
y <- seq(from=-2, to=5, length.out=175)
z <- fxy(x, y)

persp(x, y, z)
contour(x, y, z)
