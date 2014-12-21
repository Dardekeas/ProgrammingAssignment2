## Performs a matrix inversion using the solve() function.
## Beause it is calculation intensive, a cache is created and checked first.

## Cache the matrix inversion if it has already been calculated

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL        # If a new matrix is declared, the "mi" is reset
        set <- function(y) {        # Another way of replacing the matrix
                x <<- y
                mi <<- NULL        # Also resets the valuse of "mi"
        }
        get <- function() x        # Returns the original matrix
        setInverse <- function(inverse) mi <<- inverse
        # Called the first time cacheSolve() is run, stores using superassign
        getInverse <- function() mi        # Returns on subsequent accesses
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        # A list of internal functions (methods), giving labels to functions.
}

## Returns a matrix that is the inverse of 'x', checking for a cached version first.

cacheSolve <- function(x, ...) {
        mi <- x$getInverse()        # Accesses x using method
        if(!is.null(mi)) {        # If already cached (not NULL)...
                message("getting cached data")        # Alert the user
                return(mi)        # Print result, exit function
        }
        data <- x$get()        # If not cached, use the "get" method for the data
        mi <- solve(data, ...)        # Run solve on the data
        x$setInverse(mi)        # Store the result in the cache using method
        mi        # Return the Matrix Inversion
}
