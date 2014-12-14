## Put comments here that give an overall description of what your
## functions do
##
## These functions allow to compute the matrix inversion and caching
## result in order to speed up subsequent calls with the same arguments

## Write a short comment describing this function
##
## This function creates an object (list) containing data (properties)
## and functions (methods) that can cache its inverse. To be used by 
## the cacheSolve function to implement the cached matrix inversion

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setinv <- function(m) {
    inv <<- m
  }
  getinv <- function() {
    inv
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
##
## This function compute the matrix inverse of the object (list) created
## by the function makeCacheMatrix. If the inverse has been already
## computed returns the cached value, otherwise it compute, cache and
## return the matrix inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
