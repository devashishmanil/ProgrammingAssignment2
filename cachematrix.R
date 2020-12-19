## ASSIGNMENT: To create and manipulate cache of a matrix.
## makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse.
##CacheSolve: This function will return the the inverse matrix object if it is already present in cache or
## it will generate the inverse of the matrix and store that in cache for further use.

makeCacheMatrix <- function(x = matrix()) {

##Initialisation of matrix

    inverseMatrix <- NULL
    
 ## set the function for inverse matrix
    
  set <- function(y) {
          x <<- y
          inverseMatrix <<- NULL
  }
  
## getter function
  
get <- function() 
 {x }
 
 ##Setting the inverse matrix
 
 setinverseMatrix <- function(inverse)
 {
 inverseMatrix <<- inverse  ##assign the inverse matrix to the environment var
 }
 ##returning the inverse matrix
 
  getinverseMatrix <- function()
  { 
  inverseMatrix ##the getter function for inverseMatrix
 }
  ##Enclosing defined function into a list
  
  list(set = set,
       get = get,
       setinverseMatrix = setinverseMatrix,
       getinverseMatrix = getinverseMatrix)
}

## This function gets the stored inverse matrix value from makeCachematrix and returns the inverse object
##If the inverse is not available,It finds the inverse matrix and push it in cache for further use and return the new inverse object

cacheSolve <- function(x, ...) {
##getting the inverse available from the makecachematrix

  inverse <- x$getinverseMatrix()
##If the value is already calculated,Just return the cached data

  if (!is.null(inverse)) {
 
          message("Here are the cached data")
          return(inverse)
  }
  ##if the value is not already calculated, Calculate the inverse of the matrix
  ## if X is a square invertible matrix, then solve(X) returns its inverse.
  data <- x$get()
 inverse <- solve(data, ...)
 
 ##Put that value into cache for further use using setInversefunction
  x$setinverseMatrix(inverse)
  
   print("Here is the inverted matrix")
    return(inverse)
  
}

