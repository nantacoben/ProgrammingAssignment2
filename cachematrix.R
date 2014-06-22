# This R script caches an inverse of matrix

## This makeCacheMatrix function creates and returns an list container 
## that provides setters/getters for both original matrix and inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) im <<- inverse
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Given a cacheMatrix created by makeCacheMatrix, this functino returns an inverseMatrix if exist. 
## Otherwise creates a new one and returns it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(im)
  im
}

## Test
# m <- matrix(c(1, 0, 0, 1), 2, 2, byrow=TRUE)
# cm <- makeCacheMatrix(m)
# cacheSolve(cm)
