#define the sum of squared residuals
data <- read.csv("Advertising.csv")[,-1]
attach(data)
head(data)

#data[,1] <- (data[,1] - mean(TV))/sd(TV)

ssquares <- function(x) 
  {
    n <- nrow(data) # 200
    sum((data[,4] - cbind(1, data[,1]) %*% x)^2) / n
  }

# define the derivatives
derivative <- function(x) 
  {
    n <- nrow(data) # 200
    c(sum(-2*(data[,4] - cbind(1, data[,1]) %*% x)), sum(-2*(data[,1])*(data[,4] - cbind(1, data[,1]) %*% x))) / n
  }
 

# definition of the gradient descent method in 2D
gradient_descent <- function(func, derv, start, step=0.00001, tol=1e-8) 
  {
    pt1 <- start
    grdnt <- derv(pt1)
    pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
    while (abs(func(pt1)-func(pt2)) > tol) 
      {
        pt1 <- pt2
        grdnt <- derv(pt1)
        pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
        print(func(pt2)) # print progress
      }
    pt2 # return the last point
  }

# locate the minimum of the function using the Gradient Descent method
result <- gradient_descent(
  ssquares, # the function to optimize
  derivative, # the gradient of the function
  c(0,0), # start point of theplot_loss(simple_ex)  search 
  0.00001, # step size (alpha)
  1e-8) # relative tolerance for one step

# display a summary of the results
print(result) # coordinate of fucntion minimum
print(ssquares(result)) # response of fucntion minimum

