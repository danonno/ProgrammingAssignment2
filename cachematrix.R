##
## The two functions below will cache the inverse of a matrix.
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
  setInverse <- function(mean) m <<- mean
  getInverse <- function() m
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## cacheSolve  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    return(i)
  }
  dat <- x$get()
  i <- solve(dat)
  x$setInverse(i)
  i
}
