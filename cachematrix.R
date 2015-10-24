## These functions avoid recalculating an inverse matrix if it had been already calculated. This saves running time.
## makeCacheMatrix creates a matrix object and sets and gets the inverse

makeCacheMatrix <- function(x = matrix()) {
cachedInverse <-NULL
set <- function(y) {
  x <<- y
  cachedInverse <<- NULL
}
get <- function() x
setInverse <- function(inverse) cachedInverse <<- inverse
getInverse <- function() cachedInverse
list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}


## cacheSolve retrieves a cached inverse matrix if one is available, or calculates it if it doesn't
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cachedInverse <- x$getInverse()
  if(!is.null(cachedInverse)) {
    message("getting cached data")
    return(cachedInverse)
  }
  else {
 inverse <- solve(x$get())
   x$setInverse(inverse)
  }
  inverse
}
