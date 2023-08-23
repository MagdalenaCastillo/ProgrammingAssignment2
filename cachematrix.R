  ## This function creates a special matrix object that can cache its inverse

  # Create a special "matrix" object that can cache its inverse
  makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Set the matrix value
  set <- function(y) {
    # assign to the parent frame      
    x <<- y  
    # Clear the cached inverse when matrix is updated      
    inv <<- NULL  
  }
  
  # Get the matrix value
  get <- function() {
    x
  }
  
  # Set the cached inverse value
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Get the cached inverse value
  getInverse <- function() {
    inv
  }
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Compute the inverse of the cached matrix if available
cacheSolve <- function(cacheMatrix) {
  inv <- cacheMatrix$getInverse()
  
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # If inverse is not cached, calculate and cache
  data <- cacheMatrix$get()
  inv <- solve(data)
  cacheMatrix$setInverse(inv)
  
  inv
}

