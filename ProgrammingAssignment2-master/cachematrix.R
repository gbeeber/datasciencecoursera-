## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix and cacheSolve work together to take the inverse of
## a matrix and then store it in the cache.  If the inverse is there
## already, it just retrieves it without recalculating the inverse



## Write a short comment describing this function
## makeCacheMatrix computes a list with 4 functions.  First, it initializes
## the parent variables x and inv.  Then it outputs 4 functions, in one vector
## These functions set the matrix variable and retrieves it.  They also set 
## the inverse variable and retrieves it too.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x' or retrieve the inverse if it is
## already stored in the cache
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
