##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
##Below is a pair of functions that cache the inverse of a matrix.

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {inv <- NULL
set <- function(y) {
  x <<- y
  inv <<- NULL
}
get <- function() x
set.inverse <- function(inverse) inv <<- inverse
get.inverse <- function() inv
list(set = set, get = get,
     set.inverse = set.inverse,
     get.inverse = get.inverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$get.inverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  MAT <- x$get()
  inv <- solve(MAT, ...)
  x$set.inverse(inv)
  inv
}
