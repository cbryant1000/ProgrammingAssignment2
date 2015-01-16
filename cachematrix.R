##
## * * * Matrix inversion is usually a costly calculation * * *
##           [ O(n^3) for Gauss-Jordan elimination. ]
##
## * There may be an advantage to caching the inverse of a matrix, 
##     rather than computing it repeatedly.  
## * The supplied matrix is assumed to be invertible.  
## * The inverse is calculated using the R "solve" function.
##
## http://en.wikipedia.org/wiki/Computational_complexity_of_mathematical_operations
##

## This function creates a special "matrix" object 
## that can cache it's own inverse.

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) xinv <<- inv
  getinv <- function() xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function returns the inverse of the special "cached matrix" object.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinv(inv)
  inv
}
