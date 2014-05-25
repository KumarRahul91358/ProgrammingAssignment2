## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function caches the inverse matrix which is being returned by cachesolve function

makeCacheMatrix <- function(x = numeric()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# this function simply returns the inverse of a matrix
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
