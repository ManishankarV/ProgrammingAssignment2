## Matrix inversion is a costly computation. Below are two functions 
## which cache the inverse of a matrix and returns the value from cache, 
## provided the inverse was already computed.  

## The function makeCacheMatrix takes a matrix as input, calulates its inverse
## and caches the same

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The function cacheSolve returns the inverse of a matrix from cache, provided the
## inverse was already computed.If not, the inverse gets computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
