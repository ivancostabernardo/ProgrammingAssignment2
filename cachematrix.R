## The following two functions work together to calculate the inverse of a given matrix,
## caching the result and thus avoiding unnecessary calculations.

## This function returns a list with four functions:
## 'set' sets the value of the matrix;
## 'get' gets the value of the matrix;
## 'setsolve' sets the value of the inverse matrix;
## 'getsolve' gets the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv.mat <- NULL
  set <- function(y) {
    x <<- y
    inv.mat <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv.mat <<- solve
  getsolve <- function() inv.mat
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Uses the list returned by the function above.

cacheSolve <- function(x, ...) {
  inv.mat <- x$getsolve()
  ## Checks if the inverse matrix has already been evaluated and cached.
  if(!is.null(inv.mat)) {
    message("getting cached data")
    return(inv.mat)
  }
  data <- x$get()
  inv.mat <- solve(data, ...)
  x$setsolve(inv.mat)
  inv.mat
}
