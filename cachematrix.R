## Functions to speed repeaded calculations of a matrix inverse

## The makeCacheMatrix creates a new data object to mimic a matrix 
#    and store it's inverse
#   Inputs
#     - x     a standard R matrix
#   Outputs
#     - list   a list of functions implimenting the code

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL  # intiate the inverse to NULL since we don't know it
   
   # allow the user to set the matrix value
   set <- function(y) {
      x <<- y
      inv <<- NULL  # we change the value, so erase the old linverse
   }
   
   # impliment simple assignment functions
   get <- function() x
   setSolve <- function(inverse) inv <<- inverse
   getSolve <- function() inv
   
   # arange a list of functional calls
   list(set = set, get = get,
        setSolve = setSolve,
        getSolve = getSolve)
} # end makeCacheMatrix function


## The cacheSolve function looks for a stored inverese, and if it 
#     does not exist calculates a new one.
#   Inputs
#     - data     a matrix from makeCacheMatrix
#     - ...      any parameters to pass to solve()
#   Outputs
#     - inv      the invese matrix of x

cacheSolve <- function(x, ...) {
   # retrive the previous inverse, if any
   inv <- x$getSolve()
   
   if(!is.null(inv)) {
      message("getting cached data")
      return(inv) # stop here since we are done
   } # end if stored inverse exists
   
   ## we have no stored inverse, so get data and compute
   data <- x$get()
   inv <- solve(data, ...)
   x$setSolve(inv)

   # return the value
   inv 
} # end cacheSolve function
}
