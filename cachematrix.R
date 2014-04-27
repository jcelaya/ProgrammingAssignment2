## Put comments here that give an overall description of what your
## functions do

## Create a 'matrix' that also contains a cached copy of its inverse,
## as a list of functions (methods) to compute and obtain the cached data

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(newvalue) {
        x <<- newvalue
        inverse <<- NULL
    }
    get <- function() x
    
    ## It is better to check the existence of the cached value here,
    ## to encapsulate the behavior of the CacheMatrix
    getinv <- function(...) {
        if(!is.null(inverse)) {
            message("getting cached data")
        } else {
            inverse <<- solve(x, ...)
        }
        inverse
    }
    
    ## Return a list of 'methods'
    list(set = set, get = get, getinv = getinv)
}


## Return the cached value of the inverse of 'x'

cacheSolve <- function(x, ...) {
    ## Now, we just need to call getinv() on x
    x$getinv(...)
}
