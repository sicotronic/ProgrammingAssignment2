## The following two functions implement a matrix that can cache the value of
## it's inverse in order to provide a faster access to it. 
## Note: For this assignment, assume that the matrix supplied is always invertible.


## makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # set the inverse as NULL on creation or modification
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  # implement the interface functions to get the data
  get <- function() x
  
  # implement the interface functions to set the inverse
  setinverse <- function(solved) inverse <<- solved
  
  # implement the interface functions to get the inverse
  getinverse <- function() inverse
  
  list(set=set, get=get, 
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve function computes the inverse of the special "matrix"
## returned by `makeCacheMatrix` above. If the inverse has already
## been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # get the value from the special matrix data structure
  inverse <- x$getinverse()
  # return the value of the inverse if it has been already calculated
  if(!is.null(inverse)){ 
    message("getting cached inverse")
    return(inverse)
  }
  # if it wasn't calculated yet, get the matrix data first
  matrix <- x$get()
  # and then compute the inverse with the solve function provided by R
  # and cache the value in the matrix data structure and return it.
  inverse <- solve(matrix, ...)
  x$setinverse(inverse)
  inverse
}
