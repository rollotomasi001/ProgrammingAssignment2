## A pair of functions that cache the inverse of a matrix

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
  #set variable m to NULL
  m <- NULL
  
  #set function sets x to the argument y and set m to null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #return the value of x
  get <- function() x
  
  #sets m in makeCacheMatrix to solve
  setinv <- function(solve) m <<- solve
  
  #return the value of m
  getinv <- function() m
  
  #return a labeled vector of functions:
  #set, get, setinv and getinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the matrix 
## returned by makeCacheMatrix. If the inverse has already 
## been calculated (and the matrix has not changed), then 
## the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  #get the inverse from x if calculated before
  m <- x$getinv()
  
  #if not null, a valued was cached, so return m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #if null, set data to x from makeCacheMatrix
  data <- x$get()
  
  #calculate the inverse of data
  m <- solve(data, ...)
  
  #set m in x to calculated inverse
  x$setinv(m)
  
  #return inverse
  m
}