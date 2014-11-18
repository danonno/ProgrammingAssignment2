##
## Cache the inverse of a matrix.
##
## Usage:
## > x <- matrix(rnorm(8), nrow = 4)          // Create a matrix x
## > x                                         // Return the matrix
## > xcache <- makeCacheMatrix(x)              // Create the "special" matrix
## > xcache$get()                              // Return the "special" matrix
## > cacheSolve(xcache)                        // Return the inverse
## > cacheSolve(xcache)                        // Call the 2nd time, so return
##                                             // the cached inverse
##
## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.
## 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## cacheSolve  should retrieve the inverse from the cache.
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data ...")
    return(inverse)
  }
  dat <- x$get()
  inverse <- solve(dat)
  x$setInverse(inverse)
  inverse
}
