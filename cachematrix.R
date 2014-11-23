## Two functions that cache the inverse of a matrix. 

## This one creates special matrix object that is able to cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL 
  set <- function (matrix){
    x <<- matrix 
    i <<- NULL
  }
  get <- function(){
    x
  }
  setInverse <- function() {
    i
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## This one computes the inverse of the special matrix object made by makeCacheMatrix. If inverse 
## has already been calculated and is the same, retrieves inverse from cache. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data 
  x$setInverse(m)
  m
}
