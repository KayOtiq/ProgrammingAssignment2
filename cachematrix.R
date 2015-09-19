## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function the <<- operator which can be used to assign a value to an object 
## in an environment that is different from the current environment.

makeCacheMatrix <- function(x = matrix()) {
     
     #initialize the inversion variable
     nvrs <- NULL
     
     set <- function (y) {
          
          x <<- y
          
          #initialize nvrs
          nvrs <<- NULL
     }
     #cache input matrix
     get <- function() x
     
     #set inversed matrix
     set.inverse <- function(inverse) nvrs <<- inverse
     
     #get inversed matrix
     get.inverse <- function() nvrs
     
     list (set = set, 
           get = get, 
           set.inverse = set.inverse, 
           get.inverse = get.inverse)
}


## Write a short comment describing this function
## Uses the makeCacheMatrix function to compute the inverse of the matrix

cacheSolve <- function(x, ...) {
     
        ## Return a matrix that is the inverse of 'x'
     #returns nvrs from makeCacheMatrix()
     nvrs <- x$get.inverse()
     
     #if inverse has not been calculated, calculate it, else get from cache.
     if(!is.null(nvrs)) {
          message("getting cached data")
          return(nvrs)
     }
     
     data <- x$get()
     
     nvrs <- solve(data, ...)
     
     x$set.inverse(nvrs)
     
     return (nvrs)
}
