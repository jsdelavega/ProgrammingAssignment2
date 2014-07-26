## This function creates a special "matrix" object that can cache its inverse
## 

## This function creates a special matrix, that is really a list of functions:
##
## 1- set de value of the matrix
## 2- get the value of the matrix
## 3- set the value of the inverse
## 4- get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inv <- function(solve) m <<- solve
  get_inv <- function() m
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## This function computes the inverse of the matrix object returned by
## makeCacheMatrix function.
## 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get_inv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inv(m)
  m
}
