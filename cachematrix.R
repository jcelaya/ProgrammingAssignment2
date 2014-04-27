## Put comments here that give an overall description of what your
## functions do

## Create a 'matrix' that also contains a cached copy of its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function(...) {
        if(!is.null(i)) {
            message("getting cached data")
        } else {
            i <<- solve(x, ...)
        }
        i
    }
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Return the cached value of the inverse of 'x', if it
##  exists, or compute it otherwise.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv(...)
    i
}
