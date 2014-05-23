## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  #if an object is called without a method 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }  
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
  set = setsolve,
  get = getsolve)
  
}


##  This function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. 
#If the inverse has already been calculated 
#(and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x,..) {
  
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  #check for non singular matrix only then calculate mean 
  
  m <- solve(data,..)
  x$setsolve(m)
  m
  
}

