# This script was written to fulfill the Programming Assignment 2 requirements
# for the Coursera R Programming Course (rprog-14). Thanks peer reviewers!

# Function 1 - makeCacheMatrix
# 
# The following function takes a MATRIX as input and outputs a LIST of 4
# functions which allow us to set the value of the matrix, get the value of the
# matrix, set the value of the inverted matrix, and get the value of the
# inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinv <- function(solve) {
    inv <<- solve
  }
  
  getinv <- function() {
    inv
  }
  
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


# Function 2 - cacheSolve
# 
# The following function works in tandem with the output of makeCacheMatrix.
# This function first checks to see if there is a cached inversion. If one
# exists, it is returned without further calculation. If it does not exist,
# the inversion will be generated, then stored, then returned.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("Matrix previously inverted. Getting cached data!")
    return(inv)
  }
  
  message("No cached inversion! Crunching the numbers. Just a moment.")
  
  data <- x$get()
  
  inv <- solve(data, ...) ## Return a matrix that is the inverse of 'x'
  
  x$setinv(inv)
  
  inv
}