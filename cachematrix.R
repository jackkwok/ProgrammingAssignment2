## Peer-graded Programming Assignment 2 of R Programming Coursera class
## Matrix inversion is usually a costly computation.  These functions use caching to improve performance. 
## 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  x <- x
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inv) {
    invMatrix <<- inv 
  }
  getInverse <- function() {
    invMatrix
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return (invMatrix)
  }
  data <- x$get()
  invMatrix <- solve(data, ...)
  x$setInverse(invMatrix)
  invMatrix
}

## Tests

seq1 <- c(1,0,1,2,4,0,3,5,6)
mat1 <- matrix(seq1, nrow=3, ncol=3)
m1 <- makeCacheMatrix(mat1)
cacheSolve(m1)

m <- makeCacheMatrix(matrix(1:4, nrow=2))


