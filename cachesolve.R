## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates matrix and cacheSolve calculates its inverse

## Write a short comment describing this function
## makeCacheMatrix creates matrix by:
## setting value of matrix
## getting value of matrix
## setting value of inverse
## getting value of inverse
makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y) {
  x <<- y
  m <<- NULL
}
get <- function() x
setinverse <- function(inverse) m <<- inverse
getinverse <- function() m
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}
## now it retrives from cache
cachesolve <- function(x, ...) {
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