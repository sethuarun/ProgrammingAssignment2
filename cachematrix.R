## Demonstration of the capability to modify the environment objects
## through "<<-" operator.

## This function stores the supplied and its 'solve'd matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  get <- function() {
    x
  }
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getInverse <- function() {
    inverse
  }
  setInverse <- function(i) {
    inverse <<- i
  }
  list (get = get, set = set,
        getInverse = getInverse, setInverse = setInverse)
}


## This function checks whether the 'solve'd inverse is already part of the supplied x.
## If so, it returns the value from the cache, if not 'solve's the matrix, updates the cache
## and returns the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Fetching the cached inverse from the matrix.")
    inverse
  }
  inverse <- solve(x$get(),...)
  x$setInverse(inverse)
  inverse
}
