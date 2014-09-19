## These Functions solve and cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse property
  i <- NULL
  
  ## Set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## get the matrix "m"
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## Set the inverse of the matrix as a special object
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Get the inverse of the matrix "i" set above
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## create a list of methods and set names
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This fuction computes the inverse of a matrix returned by "makeCacheMatrix"
## above. If we have already calculated the inverse of the matrix before 
## and stored it through a previous run of the function (and the matrix has not
## changed), then the "cachesolve" retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Pull cached inverse of matrix "m" from "makeCacheMatrix"
  m <- x$getInverse()
  
  ## Simply return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## If matrix inverse was "Null", Get the matrix from object "get"
  ## and set to object "data" in this function
  data <- x$get()
  
  ## Calculate the inverse of matrix "data" and set to object "m"
  m <- solve(data) %*% data
  
  ## Save the inverse to the "setInverse" object created in
  ##  MakeCacheMatrix
  x$setInverse(m)
  
  ## Return the inverse matrix 
  m
}