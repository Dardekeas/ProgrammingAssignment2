## Performs a matrix inversion using the solve() function.
## Beause it is calculation intensive, a cache is created and checked first.

## Cache the matrix inversion if it has already been calculated

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) mi <<- inverse
        getInverse <- function() mi
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Returns a matrix that is the inverse of 'x', checking for a cached version first.

cacheSolve <- function(x, ...) {
        mi <- x$getInverse()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data, ...)
        x$setInverse(mi)
        mi
}
