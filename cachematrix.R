## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. These functions can help to cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.
## @param x The original R matrix
## @return The special "matrix" object
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve retrieves the inverse from 
## the cache. Note that the returned inverse matrix is not a special "matrix",
## it is just a normal R matrix.
## @param x The special "matrix" object created by the makeCacheMatrix function
## @return The inverse matrix of 'x'
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        # message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
