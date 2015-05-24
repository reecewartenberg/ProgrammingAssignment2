##This is a pair of functions that cache the inverse of a matrix for use in further computations. 

## makeCacheMatrix is a matrix-based function that stores a list of functions. 
## These are get(), set(), setsolve() and getsolve().
## To make use of listed functions one must subset makeCachMatrix.

makeCacheMatrix <- function(x = matrix()) {
       
        m <- NULL        
        
        get <- function() {
                x 
        }
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }       
        
        
        setsolve <- function(solve) {
                m <<- solve
        }
        
        getsolve <- function() {
                m
        }
        
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
        }

# cacheSolve returns the inverse of a matrix under one of two conditions:
# (1) If the matrix has been previously inverted and cached then the cached data are returned.
# (2) If the matrix has not been previously cached then the matrix is first inverted before the data are returned, a more time-consumring process. 

cacheSolve <- function(x, ...) {
        
        m <- x$getsolve()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        
}