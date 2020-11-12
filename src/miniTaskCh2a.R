# Question 1
# a)

RollBiasedDice1 <- function(n){
  counter <- numeric(n)
  dice <- numeric(n)
  
  x <- runif(n, 0, 1)
  
  for(i in 1:n){
    if(x[i] < 1/21){
      counter[i] <- 1
      dice[i] <- 1
    } else if(x[i] < 3/21){
      counter[i] <- 2
      dice[i] <- 2
    } else if(x[i] < 6/21){
      counter[i] <- 3
      dice[i] <- 3
    } else if(x[i] < 10/21){
      counter[i] <- 4
      dice[i] <- 4
    } else if(x[i] < 15/21){
      counter[i] <- 5
      dice[i] <- 5
    } else {
      counter[i] <- 5
      dice[i] <- 6
    }
  }
  
  return(list(rolls=dice, ifs=counter))
}

data1 <- RollBiasedDice1(1000)
mean(data1$ifs)

# b)

RollBiasedDice2 <- function(n){
  counter <- numeric(n)
  dice <- numeric(n)
  
  x <- runif(n, 0, 1)
  
  for(i in 1:n){
    if(x[i] > 15/21){
      counter[i] <- 1
      dice[i] <- 6
    } else if(x[i] > 10/21){
      counter[i] <- 2
      dice[i] <- 5
    } else if(x[i] > 6/21){
      counter[i] <- 3
      dice[i] <- 4
    } else if(x[i] > 3/21){
      counter[i] <- 4
      dice[i] <- 3
    } else if(x[i] > 1/21){
      counter[i] <- 5
      dice[i] <- 2
    } else {
      counter[i] <- 5
      dice[i] <- 1
    }
  }
  
  return(list(rolls=dice, ifs=counter))
}

data2 <- RollBiasedDice2(1000)
mean(data2$ifs)

# c)

mean(data1$rolls)
mean(data2$rolls)




























