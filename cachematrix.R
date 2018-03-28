## JHU Coursera R Programming Week 3 - Assignment #2
## March 2018
## author: Bushra Waheed 
## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather 
## rather than compute it repeatedly. 
## Below are a pair of functions that are used to create a special objec that stores a matrix and caches its inverse.  

## This function creates a special "marix" object that can cache is inverse. 

makeCacheMatrix <- function(mtx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mtx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mtx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function computes the inverse of the special "matrix" created by #makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then it should retrieve from the cache.

cacheSolve <- function(mtx, ...) {
  inverse <- mtx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mtx$get()
  invserse <- solve(data, ...)
  mtx$setinv(inverse)
  return(inverse)
}

