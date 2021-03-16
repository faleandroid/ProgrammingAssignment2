## Put comments here that give an overall description of what your
## functions do

## This function takes as input a matrix and creates an 'improved' version of
## a matrix that can be modified using 4 different methods.
## set: sets a new matrix value and invalidate the inverse cache
## get: returns the current matrix object
## set_inverse: sets the relative inverse of the current matrix object 'x' 
## get_inverse: returns the current and cached inverse matrix

makeCacheMatrix <- function(x = matrix()) 
{
  inverse <- NULL
  
  set <- function(new_matrix) 
  {
    x <<- new_matrix
    inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inverse <<- inverse
  get_inverse <- function() inverse
  
  list(set = set, get = get, set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## This function takes the 'improved' matrix and returns the relative inverse
## this is done by accessing (with the method get_inverse) to the current cached 
## inverse matrix.
## in the case the inverse has not been computed yet, it gets the current matrix
## computes the inverse using the 'solve' function, then set this value as future 
## cache and finally returns it.

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$get_inverse()
  
  if(!is.null(inverse)) 
  {
    message("getting cached data")
    return(inverse)
  }
  
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$set_inverse(m)
  inverse
}