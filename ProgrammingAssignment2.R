# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(matrix = matrix()) {
  inverse <- NULL
  
  set <- function(mat) {
    matrix <<- mat
    inverse <<- NULL
  }
  
  get <- function() matrix
  
  setInverse <- function(inv) inverse <<- inv
  
  getInverse <- function() inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix" and cache the result
cacheSolve <- function(cacheMatrix) {
  # Retrieve the cached inverse if available
  cachedInverse <- cacheMatrix$getInverse()
  if (!is.null(cachedInverse)) {
    message("Getting cached inverse")
    return(cachedInverse)
  }
  
  # If not cached, compute the inverse
  matrix <- cacheMatrix$get()
  inv <- solve(matrix)
  
  # Cache the inverse
  cacheMatrix$setInverse(inv)
  
  inv
}

# Example usage:
# Create a cache matrix
myMatrix <- makeCacheMatrix(matrix = matrix(c(1, 2, 3, 4), nrow = 2))

# Compute and cache the inverse
result <- cacheSolve(myMatrix)

# Retrieve the cached inverse
cachedResult <- cacheSolve(myMatrix)

