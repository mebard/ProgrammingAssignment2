## This R program contains two functions that will compute and cache the inverse of an invertible matrix

## The first function, makeCacheMatrix does the following
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse = NULL
  set = function(y) {
    x <<- y
    inverse <<- NULL
  }
  get = function() x
  setinverse = function(inverse) inverse <<- inverse 
  getinverse = function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function calculates the inverse of the matrix created with the above function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse

cacheSolve <- function(x, ...) {
  inverse = x$getinverse()

  if (!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  mat.data = x$get()
  inverse = solve(mat.data, ...)
  x$setinverse(inverse)
  
  return(inverse)
}
