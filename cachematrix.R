## These functions create a special case of a matrix which will cache it's inverse once calculated
## so that subsequent calls can pull from the cache instead of re-calculating
##
## Note that this only supports matrices that are invertible


## Function: makeCacheMatrix
##
## Description: Creates a special case matrix object that can also contains a cached inverse
##
## Parameters:
##    x:  A matrix that is invertible
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL  ## cached inverse
  
  ## On Set: store the matrix and clear the cache
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## On Get: return the matrix
  get <- function() x
  
  ## On setinvers: store the inverse in the cache
  setinverse <- function(inverse) m <<- inverse
  
  ## On getinverse: return the cache
  getinverse <- function() m
  
  ## Return list of methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function: cacheSolve
##
## Description: Calculates the inverse of a makeCacheMatrix, returning the cache if present
##
## Parameters:
##    x:  An instance of makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  ## Get and check the cache, returning if available
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Cache was not available, perform the solve and set the cache
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## EOF
