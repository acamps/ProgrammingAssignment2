# Generate a cache for storing inverted matrix and calculating when needed.

# Generate structure for a cache.
# In this case for cache inversion.
makeCacheMatrix <- function(x = matrix()) {
    # x is the matrix to be inverted
    cachedInverse <- NULL
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) cachedInverse <<- inverse
    getinverse <- function() cachedInverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# This function looks up for a matrix inverse, and if it
# is not found, it calcules it and stores it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("Getting cached data")
    } else {
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
    }
    inverse
}
