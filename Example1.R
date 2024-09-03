# Squaring elements of a given vector

square_for <- function(x){
  #is.numeric(x)
  # [ToDo] Use the for loop
  n <-  length(x)
  y <- vector(length = n)
  for (i in 1:n){ #seq_along(x) since if you do 1:n, and n = 0- error
    y[i] <-  x[i]^2
  }
  
  return (y)
}

square_sapply <- function(x){
  # [ToDo] Use the sapply function

}

square_vec <- function(x){
  # [ToDo] Use power(^) function in vector form

}

square_vec2 <- function(x){
  # [ToDo] Use multiplication(*) function in vector form

}

# [ToDo] Create a vector x of size 100,000 of normal variables


# [ToDo] Verify that all functions return the same output


# [ToDo] Use microbenchmark package to compare three functions in terms of speed
