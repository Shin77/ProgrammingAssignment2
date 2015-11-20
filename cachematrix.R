## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its inverse.
## The function stores a list of other functions.
## Specifically, there are four functions (set, get, setinv and getinv) stored in the main function.

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinv <- function(inv) invx <<- inv
  getinv <- function() invx
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix.
## If this has been already calculated and the original matrix has not been changed, then
## the code retrieve the data stored in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
  invx <- x$getinv()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinv(invx)
  invx
 
}
