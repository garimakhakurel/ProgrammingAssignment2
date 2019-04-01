## Functions to save the inverse of a matrix in cache

## makeCacheMatrix contains a list of functions to set and get the values of a matrix 
## using setMatrix and getMatrix
## & to assign and retrive the vlaue of inverse of the matrix using 
## setInverse and getInverse respectively

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setMatrix <- function(y) {
  x <<- y
  i <<- NULL
  }
  getMatrix <- function() x
 
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve returns the inverse of the matrix from the memory if it was stored 
## earlier otherwise, uses solve function to calculate and returns the inverse

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("Cached Inverse")
    return(i)
  }
  matrix <- x$getMatrix()
  i <- solve(matrix, ...)
  x$setInverse(i)
  i
}
