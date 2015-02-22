## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" which is really a list containing functions to
# 1. get/set the value of the matrix
# 2. get/set the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(value) {
    x <<- value
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Calculates and caches the inverse of the special "matrix" created with the above function if inverse isn't there, otherwise returning the previously cached inverse. 
cacheSolve <- function(x, ...) {
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
