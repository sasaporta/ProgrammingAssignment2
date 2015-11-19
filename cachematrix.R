#v = makeVector(c(1, 2, 3))
#v2 = makeVector(c(1, 2, 3, 3, 3))
#cachemean(v)
#cachemean(v)
#cachemean(v2)
#m <- matrix(c(1, 7, 2, 4, 6, 8, 2, 4, 5), 3, 3)
#solve(m)

## Functions makeCacheMatrix and cacheSolve can be used together to compute
## the inverse of a matrix. Once a particular matrix's inverse has been computed,
## the result is cached, so that the computation need not be repeated for the
## same matrix in the future.
##
## Example usage:
## m1 = makeCacheMatrix(matrix(c(1, 7, 2, 4, 6, 8, 2, 4, 5), 3, 3))
## m2 = makeCacheMatrix(matrix(c(999, 7, 2, 4, 6, 8, 2, 4, 5), 3, 3))
## cacheSolve(m1)
## cacheSolve(m1)
## cacheSolve(m2)
## Each call to cacheSolve() will return a 3x3 matrix. The second call will also
## output "getting cached data" and will avoid recomuting the inverse.


## Creates a special "vector" of functions to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(mean) inv <<- mean
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Returns the inverse of the specified matrix, using a cached value if the
## matrix's inverse has previously been calculated. The value of x must be an
## invertible matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
