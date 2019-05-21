## Put comments here that give an overall description of what your
## functions do

## this function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x  
  setinverse <- function(solve) 
    m <<- solve
  getinverse <- function()  m
  list(set = set, get = get, ##give names to the list of functions
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned
##by makeCacheMatrix function. It first checks to see if the inverse has 
##already been calculated. If so, it gets the inverse from the cache and skip
##the calculation. Otherwise, it computes the inverse of the new matrix and 
##set the value in the cache.

cacheSolve <- function(x, ...) { ## input argument is result from makeCaheMatrix
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}



