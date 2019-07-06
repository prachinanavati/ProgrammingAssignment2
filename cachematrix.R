## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  matinverse <- NULL
  set <- function(y) { ## changes the matrix stored in makeCacheMatrix 
    x <<- y
    matinverse <<- NULL
  }
  get <- function() x ## returns matrix x which is stored in makeCacheMatrix
  setInverse <- function(inverse) matinverse <<- inverse ## stores the value of the input in matinverse
  getInverse <- function() matinverse ## returns the value of matinverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matinverse <- x$getInverse()
  if (!is.null(matinverse)) {
    message("Getting cached data")
    return(matinverse)
  }
  mat <- x$get()
  matinverse <- solve(mat, ...) ## it computes the inverse of the matrix
  x$setInverse(matinverse)
  matinverse
}
