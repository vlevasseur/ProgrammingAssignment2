## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## ------------------------------------------------
## This function returns a special "matrix" object
## that can cache its inverse.
## There is only one parameter X, tha is by default
## an empty Matrix
## -----------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      
      ## Here, we assign to X a value 
      set <-function(y){
        x <<- y
        m <<- NULL      
      }
      
      ## Here, we get the value of x
      get <- function() x
      
      ## Here we enable set and get methods for Inverse
      setInverse <- function(inverse) m <<- inverse
      getInverse <- function() m
      
      ## Here we list methods
      list(set = set, get=get, setInverse = setInverse, getInverse=getInverse)    
}


## Write a short comment describing this function
## ------------------------------------------------------------
## In this function we make the Inverse of the
## special "Matrix" returned by the parameter "makeCacheMatrix
## Attenion ! if the inverse has been already calculated :
##    return cachesolve (from the cache)
## ------------------------------------------------------------

cacheSolve <- function(x, ...) {
      m <- x$getInverse() 
      
      ## Here we test if Inverse is already calculated
      ## if not null we return the cachesolve
      if(!is.null(m)){
        message("Getting cached data called cachesolve:")
        return(m)
      }
      
      ## Return a matrix that is the inverse of 'x' if not in cache
      data <-x$get()
      
      ## Compute the Inverse
      m <- solve(data,...)
      
      ## Put the result in the cache
      x$setInverse(m)
      
      ## Return the value
      m
}
