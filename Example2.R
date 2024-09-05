# Classification rule in discriminant analysis
require(mnormt) # for multivariate normal data generation

# Functions for classification
##############################################
# INPUT
# beta - supplied discriminant vector
# xtrain, ytrain - training data
# xtest, ytest - testing data
# xtrain and xtest are matrices with p columns, 
# ytrain and ytest are vectors; labels are 1 and 2
#
# OUTPUT
# ypred - predicted class membership
# error - percent of misclassified observations

classify_for <- function(beta, xtrain, ytrain, xtest, ytest){
  # [ToDo] Code discriminant analysis classifier using for loop
  
  # Calculate sample means based on training data
  xbar1 <- colMeans(xtrain[ytrain = 1, ])#drop = FALSE
  xbar1 <- colMeans(xtrain[ytrain = 2, ])#drop = FALSE
  
  # Calculate class assignments for xtest in a for loop
  npred <- nrow(xtest)
  ypred <- rep(1, npred) #default is everything to be group 2
  for ( i in 1:npred){
    #Use h(x) rule to predict- see which of h1 and h2 is smaller- smaller means its the group
    h1 = as.numeric((crossprod(xtest[i, ]-xbar1, beta))^2) #calculate projection onto the line
    h2 = as.numeric((crossprod(xtest[i, ]-xbar2, beta))^2)
    if (h2 < h1){
      ypred[i] <- 2
    }
    
  }
  # Calculate % error using ytest
  error <- 100*mean(ypred != ytest) #sum(ypred != ytest)/npred
  
  # Return predictions and error
  return(list(ypred = ypred, error = error))
}

#beta represents the place of separation which distinguishes class 1 and class 2

classify_vec <- function(beta, xtrain, ytrain, xtest, ytest){
  # [ToDo] Try to create vectorized version of classify_for
  
  # Calculate sample means based on training data
  xbar1 <- colMeans(xtrain[ytrain == 1, ]) # drop= FALSE
  xbar2 <- colMeans(xtrain[ytrain == 2, ])
  
  # Calculate the inner product of the means with beta
  m1b <- as.numeric(crossprod(xbar1, beta))
  m2b <- as.numeric(crossprod(xbar2, beta))
  
  # Calculate produce of xtest with beta
  xtestb <- xtest%*%beta
  
  # Calculate class assignments for xtest using matrix and vector algebra
  h1 <- (xtestb - m1b)^2
  h2 <- (xtestb - m2b)^2
  
  ypred <- as.numeric((h2 < h1) + 1) # ifelse() #by default h2<h1 will give either 1 or 0, so we add 1 to get class labels 1 and 2

  
  # Calculate % error using ytest
  error <- 100*mean(ypred != ytest)
  
  # Return predictions and error
  return(list(ypred = ypred, error = error))
}


  #error <- 100*mean(ypred != ytest)
# Example 
##############################################

# Create model parameters
p <- 10 # dimension
mu1 <- rep(0, p) # mean vector for class 1
mu2 <- rep(1, p) # mean vector for class 2
# equicorrelation covariance matrix with correlation rho
rho <- 0.4
Sigma <- matrix(rho, p, p) + diag(1-rho, p) # we assume in lda that each class has same covariance

# Create training data
n1 <- 100 # number of samples in class 1
n2 <- 100 # number of samples in class 2
ytrain <- c(rep(1, n1), rep(2, n2)) # class assignment
xtrain <- matrix(0, n1 + n2, p)
xtrain[ytrain == 1, ] <- rmnorm(n1, mean = mu1, varcov = Sigma)
xtrain[ytrain == 2, ] <- rmnorm(n2, mean = mu2, varcov = Sigma)

# Create testing data of the same size for simplicity
ytest<- c(rep(1, n1), rep(2, n2)) # class assignment
xtest <- matrix(0, n1 + n2, p)

xtest[ytest == 1, ] <- rmnorm(n1, mean = mu1, varcov = Sigma)
xtest[ytest == 2, ] <- rmnorm(n2, mean = mu2, varcov = Sigma)

# Calculate sample means and within class sample covariance on training data
xbar1 <- colMeans(xtrain[ytrain == 1, ])
xbar2 <- colMeans(xtrain[ytrain == 2, ])
W <- ((n1 - 1) * cov(xtrain[ytrain == 1, ]) + (n2 - 1) * cov(xtrain[ytrain == 2, ]))/(n1 + n2 - 2)

# Calculate the discriminant vector
beta <- solve(W, xbar1 - xbar2)

# Calculate test assignments based on each function

out1 = classify_for(beta, xtrain, ytrain, xtest, ytest)

out2 = classify_vec(beta, xtrain, ytrain, xtest, ytest)

# [ToDo] Verify the assignments agree with each other
testthat:: expect_equal(out1, out2)
summary(out1$ypred-out2$ypred)

# [ToDo] Use microbenchmark package to compare the timing
library(microbenchmark)

microbenchmark(
  classify_for(beta, xtrain, ytrain, xtest, ytest)
  classify_vec(beta, xtrain, ytrain, xtest, ytest)
)

library(microbenchmark)