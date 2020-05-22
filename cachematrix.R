## These functions cache the inverse of a matrix
## If it was calculated, it will cache, if not, it will calculate

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inversm <- NULL
  set <- function(y) {
    x <<- y
    inversm <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inversm <<- solve
  getinv <- function() inversm
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## if the inverse has already been calculated then it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  inversm <- getinv(x)
  if(!is.null(inversm)) {
    message("getting cached data")
    return(inversm)
  }
  data <- get(x)
  inversm <- solve(data)
  x[setinv(inversm)]
  inversm
}