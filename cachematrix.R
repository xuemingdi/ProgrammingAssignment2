## Put comments here that give an overall description of what your
## functions do

## The function is to make a cache matrix object.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will output inverse of input matrix by using of function makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv))
  {
    message("get inverse matrix")
    return(inv)
  }
  data<- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}



