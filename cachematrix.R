## Set of two Functions for Caching the Inverse of a Matrix

## This function CacheMatrix creates a special "matrix" object that can cache its inverse

## The special "matrix" object is really a list containing a function to

##  1. set the value of the vector
##	2. get the value of the vector
##	3. set the value of the inverse
##	4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    # inverse already exist, return the cached value
    message("getting cached data")
    return(inv)
  }
  # Calculate the inverse as it does not exist
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
