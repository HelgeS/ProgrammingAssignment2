## Solution to programming assignment 2
## makeCacheMatrix creates the cachable matrix object
## cacheSolve returns the inverse for such an object (either from cache or calculated)

## Creates an extended matrix object, which can store (= cache) its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL # Cached value, initially not calculated
  set <- function(y) { # Set a new matrix
    x <<- y
    inverse <<- NULL
  }
  get <- function() x # Return the original matrix
  setinverse <- function(inv) inverse <<- inv # Store the inverse -> Caching
  getinverse <- function() inverse # Return a cached inverse (or NULL)
  # Return a list containing the previously defined functions -> Object representation
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function accepts as a parameter the return value of makeCacheMatrix.
## It tries to get the cached inverse of the matrix. If it succeeds the cached value is used,
## otherwise the inverse is calculated and stored in the object.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse() # Try to get the already calculated inverse
  
  # Was the inverse already calculated?
  if (!is.null(inv)) {
    # Use cached data
    message("getting cached data")
    return(inv)
  }
  
  # The inverse wasn't yet calculated

  data <- x$get() # Get the original matrix
  inv <- solve(data, ...) # Calculate the inverse
  x$setinverse(inv) # Store the value so it can be re-used next time
  inv # Return calculated inverse
}