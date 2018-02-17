## Put comments here that give an overall description of what your
## functions do
# Author: Xiuyuan Li 


## Write a short comment describing this function
# The function is used to get a matrix as an input, set the value of the matrix. 
# Operator is used to assign a value to an object that is different from the current environment.

# Input a value 
makeCacheMatrix <- function(x = matrix()) {
  invMtx <- NULL
  # set the value of the Matrix 
  setMtx <- function(y){
    x <<-y
    invMtx <<-NULL
  }
  # get the value of the Matrix
  getMtx <-function() x 
  # set the value of the Invertible matrix
  setInv <- function(inverse) invMtx <<- inverse
  # get the value of the invertible matrix
  getInv <- function () invMtx 
}


## Write a short comment describing this function
# The following function takes the output of the previous matrix makeCacheMatrix
# as an input and checks inverse matrix from the previous function has any value. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMtx <- x$getInverse()
  if (!is.null(invMtx)) {
    message("Cached Invertible Matrix")
    return (invMtx)
  }
  MtxDt <- x$getMatrix()
  invMtx <- solve(MtxDt,...)
  x$setInverse(invMtx)
  return(invMtx)
}




