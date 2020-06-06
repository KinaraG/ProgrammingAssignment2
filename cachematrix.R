## Caching the Inverse of the matrix.
## Here below functions will be usedd to create a special object which will store a
## matrix and caches its inverse.
## 
## makeCacheMatrix will create an matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function ()x
  setInverse <- function(inverse)i <<- inverse
  getInverse <- function()i
  list(set = set, get = get, setInverse = setInverse, getInverse=getInverse)
}


## This function will calculate the inverse of matrix. If the inverse is already 
## created then it will retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat,...)
  x$setInverse(i)
  i
}
