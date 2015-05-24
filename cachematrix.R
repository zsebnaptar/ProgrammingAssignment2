## Cache the inverse of a matrix.

## creates a special "matrix" object that can cache its inverse.

## contains the following functions:
## set: set the value of the matrix
## get: get the value of the matriw
## setinverse: set the value of the inverse
## getinverse: get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {                         ## if the inverse was cached
    message("getting cached data")
    return(i)                               ## return without recalculate the inverse
  }
  data <- x$get()
  i <- solve(data, ...)                     ## calculate the inverse                
  x$setinverse(i)
  i                                         ## return the inverse
}
