# Function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # Initialize the inverse matrix
    inverse <- NULL
    
    # Function to set the matrix
    set <- function(y) {
        x <<- y
        # Invalidate the cached inverse
        inverse <<- NULL
    }
    
    # Function to get the matrix
    get <- function() x
    
    # Function to get the cached inverse
    getInverse <- function() inverse
    
    # Function to compute the inverse and cache it
    cacheSolve <- function(...) {
        # Check if the cached inverse is available
        if (!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
        }
        
        # Compute the inverse
        inverse <- solve(x, ...)
        # Cache the inverse
        inverse <<- inverse
        # Return the inverse
        inverse
    }
    
    # Return a list of functions
    list(set = set, get = get, getInverse = getInverse, cacheSolve = cacheSolve)
}

# Function to compute the inverse of the special matrix
cacheSolve <- function(x, ...) {
    # Get the cached inverse
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # Compute the inverse and cache it
    inv <- x$cacheSolve(...)
    inv
}
