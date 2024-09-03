# Squaring elements of a given vector

square_for <- function(x){
  #is.numeric(x)
  # [ToDo] Use the for loop
  n <-  length(x)
  y <- vector(length = n) #c() is bad for performance since you are always reallocating memory
  
  for (i in 1:n){ #seq_along(x) since if you do 1:n, and n = 0- error
    y[i] <-  x[i]^2
  }
  
  return (y)
}

square_sapply <- function(x){ #vectorized function is not necessarily faster- we need to benchmark
  # [ToDo] Use the sapply function
  sapply(x, \(x){x^2}) #anonymous function x^2, which doesn't get saved except for locally
}

square_vec <- function(x){
  # [ToDo] Use power(^) function in vector form
  x^2
}

square_vec2 <- function(x){
  # [ToDo] Use multiplication(*) function in vector form
  x*x
}

# [ToDo] Create a vector x of size 100,000 of normal variables
x = rnorm(100000, mean = 0, sd = 1)

# [ToDo] Verify that all functions return the same output
library(testthat)
expect_equal(square_vec(x), square_sapply(x))

# [ToDo] Use microbenchmark package to compare three functions in terms of speed
library(microbenchmark)
microbenchmark(
  square_for(x), 
  square_sapply(x), 
  square_vec(x),
  square_vec2(x),
  times = 10 #limits the amount of times we do this
)
