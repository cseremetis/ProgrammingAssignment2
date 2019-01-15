## The goal is to create an object that can cache the inverse of
## a matrix to facilitate future computations

## Creates a "matrix" that's really a list with an internal matrix x
makeCacheMatrix <- function(x = matrix(numeric())) {
  inv <<- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <- inverse
  getInverse <- function() inv
  
  list(set = set, get = get, 
       setInverse = setInverse, getInverse = getInverse)
}


## Checks if the inverse of the matrix is stored in the cache,
## and computes it otherwise
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    return(inv)
  }
  
  inf <- x$get()
  inv <- solve(inf)
  x$setInverse(inv)
  inv
}
