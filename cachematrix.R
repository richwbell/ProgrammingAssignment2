## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverted<- NULL
  create <- function(y){
    x<<- y 
    inverted<<- NULL
  }
  get<- function() x
  setInverted <- function(invert) inverted<<- invert
  getInverted <- function() inverted
  list(create = create, get=get, setInverted=setInverted, getInverted=getInverted)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverted<- x$getInverted()
  if(!is.null(inverted)){
    message("Getting data from Cache")
    return(inverted)
  }
  workspace<- x$get()
  inverted<- solve(workspace, ...)
  x$setInverted(inverted)
  inverted
}
