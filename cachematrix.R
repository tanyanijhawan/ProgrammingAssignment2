##Put comments here that give an overall description of what your
## functions do
## My aim in this project is to create two functions with the names 
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix and give the desired result

## Write a short comment describing this function
## makeCacheMatrix is a function that helps us create a special object of the matrix
## that in turn caches its inverse for the input 

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve is a function which computes the inverse of the special matrix 
## which is returned by the function makeCacheMatrix. If the inverse has already been calculated 
## ,then the cachesolve should retrieve the inverse (from the cache)

cacheSolve <- function(x, ...) {
  ## Returning a matrix that is the inverse of "x"
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Cached Result Displayed")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}