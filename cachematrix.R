## This pair of functions creates a matrix, inverts it, and caches the inversion. 
## The purpose is to save the computational power of repeatedly inverting the 
## same matrix.


## This function creates a list vector which contains a matrix vector and several sub-functions. 
## When used with the second function, this function stores the inversion of the first matrix.

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


## This function recieves the function above as input. It will either invert
## the matrix vector contained within, or output the pre-cached inverted value. 


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
