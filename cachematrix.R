## makeCacheMatrix and cacheSolve cache the value of the inverse matrix so that when needed, the inverse
## matrix can be looked up in the cahce rather than recomputed, saving time.

## makeCacheMatrix returns a list containing functions to:
## set value of matrix
## get value of matrix
## set value of inverse
## get value of inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { 
    x <<- y
    m <<- NULL
  }
  get <- function() x 
  setinverse <- function(solve) m <<- solve 
  getinverse <- function() m 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve either returns the previously calculated inverse matrix from the cache, or calculates the inverse
## matrix and sets its value in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) { #if mean has already been calculated, return it
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) #otherwise calculate the mean
  x$setinverse(m) #set the value of the mean in the cache
  m
}
