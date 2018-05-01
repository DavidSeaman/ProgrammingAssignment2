## Programming Assignment 2 - David Seaman
#  This program provides two functions useful for caching the inverse of a matrix rather 
# than having to recalculate the inverse matrix every time it is required
# This code uses a newly introduced operator <<- to store a value in a different environment


## The function makeCacheMatrix creates an object with functions to set and get 
# the inverse value of the matrix

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

## CacheSolve returns the inverse of a matrix from the cached version if it has been created for
# the variable x, or if it doesn;t exist, the inverse will be calculated and stored within the 
# variable

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}