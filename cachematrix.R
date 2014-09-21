## Creates a special matrix computes it's inverse and caches it for 
## use over and over again.

## Create the matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ##define a null matrix
  i <- NULL
  ##function for setting the matrix values
  setmat<-function(y){
    x <<- y
    i <<- NULL
  }
  ##function for getting the value of the matrix
  getmat <- function() x
  ##function for setting inverse to variable
  setinverse <- function(s) i <<- s
  ##function for getting inverse variable
  getinverse <- function() i
  ##return list of functions
  list(setmat=setmat,getmat=getmat,setinverse=setinverse,getinverse=getinverse)
}


## Compute the inverse of the matrix returned by makeCacheMatrix.
##If the inverse is already cached, it will return the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ##assign the inverse matrix from makeCacheMatrix to i
  i <- x$getinverse()
  ##determine if null, if not, then result is coming from cached data
  ##Return inverse
  if(!is.null(i)){
      message("Inverse is coming from cached data")
      return(i)
  }
  ##if inverse is null, then calcualte inverse of matrix
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
