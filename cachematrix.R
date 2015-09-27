## The following functions create a special matrix that can keep cache of the matrices data and its inverse
## This function "makeCacheMatrix" creates a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      matrix <- NULL
     
       set <- function(y) 
      {
            x <<- y
            matrix_inv <<- NULL
       }
       
      get <- function() x
      set_inverse <- function(inverse) matrix_inv <<- inverse
      get_inverse <- function() matrix_inv
      
      list(set = set, get = get, 
           set_inverse = set_inverse,
           get_inverse = get_inverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. 
## This function first check if the "matrix object" has cached inversion, 
## if this is the case, it will simply return it.  Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse in the cache 
## using the `set_inverse` function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      matrix_inv <- x$get_inverse()
      if(!is.null(matrix_inv)) {
            message("getting cached data")
            return(matrix_inv)
      }
      data <- x$get()
      matrix_inv <- solve(data)
      x$set_inverse(matrix_inv)
      matrix_inv
}