## This function create a special matrix
## which is really a list of functions
## functions do
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x<<-y
    inverse<<- NULL
  }
  
  get <-function() x
  setinverse <- function(mat) inverse<-mat
  getinverse <- function() inverse
  list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of the special matrix created by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("hetting cache inverse")
    return(inverse)
  }
  
  data<-x$get()
  inverse<-solve(data,...)
  x$setinverse(inverse)
  inverse
}
