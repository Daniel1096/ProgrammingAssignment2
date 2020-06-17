## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##normally complicated processes tend to have significant time and memory expenses, 
##we are going to try to account for cache expenses and see how much it is to invert a matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
##this function helps us to solve the inverse of a matrix
##This helps us because it is a very expensive action to do by hand
cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
