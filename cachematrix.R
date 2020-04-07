## The functions below can cache the inverse of a matrix

##This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(matrix1 = matrix()) {
  inv <- NULL
  set <- function(y) {
    matrix1 <<- y
    inv <<- NULL
  }
  get <- function() matrix1
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrix1 <- x$getInverse()
  if(!is.null(matrix1)) {
    message("getting cached data")
    return(matrix1)
  }
  data <- x$get()
  inv <- solve(data) %*% data
  x$setInverse(inv)
  inv
}