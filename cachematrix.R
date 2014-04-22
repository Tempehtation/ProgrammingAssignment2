## The function makeCacheMatrix creates a special matrix that can cache its inverse. 
## The function cacheSolve either calculates the inverse of the special matrix in the makeCacheMatrix function or obtains the cached inverse if it has already been calculated.

## This function creates a matrix object, and can cache the inverse of this matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve 
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## This function computes the inverse of the matrix returned by the makeCacheMatrix 
## function. If the matrix has not changed and the inverse has already been calculated, 
## the cacheSolve function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}       
        
        
