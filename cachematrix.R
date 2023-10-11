


# This function creates a list containing methods for setting, getting, and caching an inverse of a matrix.
# The list acts as a closure scope, allowing these methods to have access to and modify the local variables `x` and `inv`.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize a variable to hold the cached inverse of the matrix
  
  # This method allows the user to set the matrix `x`.
  set <- function(y) {
    x <<- y  # Assign the new matrix to `x`
    inv <<- NULL  # Reset the cached inverse whenever the matrix is updated
  }
  
  # This method allows the user to retrieve the matrix `x`.
  get <- function() x
  
  # This method allows the user to set the cached inverse of the matrix.
  setInverse <- function(inverse) inv <<- inverse
  
  # This method allows the user to retrieve the cached inverse of the matrix.
  getInverse <- function() inv
  
  # Return a list of the methods, allowing them to be accessed and used outside of this function.
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)
}



# This function computes the inverse of a matrix using a caching mechanism to store and reuse previous results.
# If the inverse has already been computed and cached, it retrieves and returns the cached inverse.
# If the inverse has not been cached, it computes the inverse, caches it, and then returns it.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Try to retrieve the cached inverse
  if(!is.null(inv)) {
    message("getting cached data")  # Notify the user that the cached data is being used
    return(inv)  # Return the cached inverse
  }
  data <- x$get()  # Retrieve the matrix from the `x` list
  inv <- solve(data, ...)  # Compute the inverse of the matrix
  x$setInverse(inv)  # Cache the computed inverse
  inv  # Return the computed inverse
}




## Usage:
## Create a cached matrix
my_matrix <- makeCacheMatrix(matrix(c(1,2,3,4), 2, 2))

## Get the inverse, caching the result
inverse_matrix <- cacheSolve(my_matrix)
print(inverse_matrix)

## Get the inverse again (this time it retrieves the cached inverse)
inverse_matrix <- cacheSolve(my_matrix)
print(inverse_matrix)  # prints the inverse matrix, with a message "getting cached data"


