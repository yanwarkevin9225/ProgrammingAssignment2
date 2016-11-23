## Put comments here that give an overall description of what your functions do.

## Write a short comment describing this function
## makeCacheMatrix function creates a matrix that has two public access methods (PAM), i.e. set and get functions.
## The first PAM is to set and get the matrix value.
## The second PAM is to set and get the inversed matrix value.
## Assumption: the matrix supplied is always invertible (i.e. square matrix).

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve function calculates and displays the inverse result of a given matrix.
## Firstly, it retrieves the "inverse" variable value and checks if this variable is null or not.
## If "inverse" variable is not null, then the function displays "inverse" variable value and does not do the inverse calculation again.
## If "inverse" variable is null, the function calculates the inverse result, sets the inverse value to "inverse" varibale, and displays "inverse" variable value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    message("Getting cached inversed matrix data.")
    return(inverse)
  }
  
  inverse <- solve(x$get(), ...)
  x$setInverse(inverse)
  inverse
}
