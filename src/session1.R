#######################################################
# Introduction to Programming in R

##########-------------------------------------------
# EXERCISE 1

sums <- numeric(100)
sums[1] <- 1
for (i in 2:100){
  sums[i] <- sums[i-1] + i
}

#########-----------------------------------------------
# EXERCISE 2

samples <- numeric(100)
for (i in 1:100){
  samples[i] <- rnorm(1, i, i^2)
}

#########---------------------------------------------
# EXERCISE 3

x <- runif(1, 0, 1)
if(x < 1/6){print(1)
  }else if(x >= 1/6 & x < 2/6){print(2)
  }else if(x >= 2/6 & x < 3/6){print(3)
  }else if(x >= 3/6 & x < 4/6){print(4)
  }else if(x >= 4/6 & x < 5/6){print(5)
  }else print(6)

##########--------------------------------------------
# EXERCISE 4

# wtf is this question?

#########---------------------------------------
# EXERCISE 5

i <- 0
total <- 0
while(total<50){
  i <- i + 1
  total <- total + runif(1, 0, 1)
}
print(i)

#########-------------------------------------
# EXERCISE 6

counter <- 0
while(runif(1,0,1) < 0.5){
  counter <- counter + 1
}
if(counter %% 2 == 0){
  print("Player 1 wins")
} else print("Player 2 wins")

##########------------------------------------
# EXERCISE 7

dice <- function(){
  x <- runif(1, 0, 1)
  if(x < 1/6){return(1)
    }else if(x >= 1/6 & x < 2/6){return(2)
    }else if(x >= 2/6 & x < 3/6){return(3)
    }else if(x >= 3/6 & x < 4/6){return(4)
    }else if(x >= 4/6 & x < 5/6){return(5)
    }else return(6)
}
dice()

########-----------------------------------
# EXERCISE 8

nDice <- function(n = 2){
  for(i in 1:n){
    print(dice())
  }
}

nDice(2)

##############################################
# Introduction to R

##############################################
# Session 1: Additional Exercises

########----------------------------------------
# Exercise 5

counter <- function(p){
  x <- 0
  while(runif(1, 0, 1) > p){
    x <- x + 1
  }
  
  return(x)
}

simulator <- function(N){
  return(sapply(runif(N, 0, 1), counter))
}

data <- simulator(100000)
print(data)
hist(data)
print(mean(data))


########-----------------------------------------------
# Exercise 6






















