## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Initialize a variable to store the cached inverse
  inverse <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    # Reset the cached inverse
    inverse <<- NULL
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to get the cached inverse
  getInverse <- function() inverse
  
  # Function to cache the inverse of the matrix
  cacheInverse <- function(solve_matrix) {
    inverse <<- solve_matrix
  }
  
  # Return a list of functions
  list(set = set, get = get, getInverse = getInverse, cacheInverse = cacheInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(cacheMatrix, ...) {
  # Retrieve the cached inverse if available
  cached_inverse <- cacheMatrix$getInverse()
  
  # If the cached inverse exists, return it
  if (!is.null(cached_inverse)) {
    message("Returning cached inverse")
    return(cached_inverse)
  }
  
  # If the cached inverse doesn't exist, compute it
  matrix_to_inverse <- cacheMatrix$get()
  inverse <- solve(matrix_to_inverse, ...)
  
  # Cache the inverse
  cacheMatrix$cacheInverse(inverse)
  
  # Return the computed inverse
  inverse
}

