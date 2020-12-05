## create a makeCacheMatrix function to create a matrix and cached its inverse by computing first time.

## Write a short comment describing this function
makeCacheMatrix <- function(x=matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(matrix11) inverse <<- solve(matrix11)
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## when user want to get inverse of matrix first check it cached then return from it otherwise compute it and return.
cacheSolve <- function(x,...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}





