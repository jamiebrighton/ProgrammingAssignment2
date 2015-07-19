## These functions calculate the inverse of a matrix and store it in a cache
## If the inverse has already been calculated then it is read from the cache

## The first function makeCacheMatrix creates a special "vector" and
## sets up the functions needed to interact with it
## get, set, setinverse, getinverse

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

## cacheSolve calculates the inverse of the special "vector" created
## in makeCacheMatrix. It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                ## If value is in cache then return a message and the value and exit
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        ## Assume that the matrix is always invertible for this exercise
        m <- solve(data, ...)
        x$setinverse(m)
        m
}