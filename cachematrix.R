## Functions perform tasks required for Assignment 2 (Lexical Scoping).
## makeCacheMatrix returns a list containing functions for setting and getting the matrix in
## addition to setting and getting the inverse.
## Usage:
## M <- matrix(c(10,11,12,13,14,15,2,3,4), nrow=3, ncol=3)
## cacheMatrix <- makeCacheMatrix(M)
## cacheSolve(cacheMatrix)

## makeCacheMatrix accepts a matrix parameter, x, and returns a list, with members for 
##setting and getting the matrix in addition to setting and getting the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,m,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Return the inverse of the matrix cached by the makeCacheMatrix function
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  data <- x$get()
  if(!is.null(m) & !identical(x,data)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  print(data)
  flush.console()
  m <- solve(data)
  x$setinverse(m)
  m
}
