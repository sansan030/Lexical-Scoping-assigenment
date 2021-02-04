## this is an asshignment of courses on coursera 
## matrix inversion is usually a costly computation
## caching the inverse of a matrix benefits 
## below are a pair of functions to create a special object storing a matrix and caches its inverse

## the first function, makeCacheMatrix creates special matrix to cache its inverse
library(GoFKernel)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set,get=get,
       setInverse=setInverse,
       getInverse=getInverse)
  
}


## the second function, cacheSolve compute the inverse of matrix created by makeCacheMatrix function above.
## it first check if the inverse has already been calculated, if so, it gets inverse from cache
## otherwise, it calculates the inverse and set value of inverse in the cache
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message('getting cached data')
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
