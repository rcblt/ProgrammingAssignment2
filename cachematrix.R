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

  # define the 'method' set to initialize
  # the matrix to the parameter received and
  # the inverse to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  # define the 'method' get to retrieve
  # the current matrix
  get <- function() {
    x
  }

  # define the 'method' setinv to initialize
  # the value of the inverse matrix
  # probalbly never actually used
  setinv <- function(m) {
    inv <<- m
  }

  # define the 'method' getinv to retrieve
  # the value of the inverse matrix
  getinv <- function() {
    inv
  }

  # define the object to be returned
  # as a list of 'methods' to set/get
  # the matrix and its inverse
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

  # retrieve the inverse matrix
  inv <- x$getinv()

  # if the inverse is not NULL 
  # returns the cached values and quit
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  # no inverse matrix is cached so
  # compute the inverse by getting
  # the matrix and using the function
  # solve to inverse it
  data <- x$get()
  inv <- solve(data)

  # set the inverse matrix for future
  # use and returns it
  x$setinv(inv)
  inv
}
