## This source file contains a pair of functions:
##   makeCacheMatrix
##   cacheSolve
## These functions allows one to work with a matrix and cache the inverse.
## Usage example:
##   x <- makeCacheMatrix(matrix(1:4, 2, 2))
##   y <- cacheSolve(x) + 1
##   z <- cacheSolve(x)^2


##   makeCacheMatrix creates a new object which represents a matrix and
## its inverse. The output is a list with the following function elements:
##     (a) get/set functions which allows one to get and set the matrix itself.
##           Setting a new matrix automatically clears the cached inverse.
##     (b) getSolved/setSolved functions which allows one to get and set the
##           matrix's inverse
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(newX) {
        x <<- newX
        s <<- NULL
    }
    get <- function() x
    setSolved <- function(solved) s <<- solved
    getSolved <- function() s
    
    list(get=get, set=set, getSolved=getSolved, setSolved=setSolved)
}

##   cacheSolve takes an object returned by makeCacheMatrix and returns the
## inverse of the stored matrix. If cached value is available it will be used.
## Otherwise the inverse matrix will be computed, stored in cache and returned.
##   Important note: if additional arguments are passed to cacheSolve, then
## these will be passed to the underlying call of solve function for the
## computation. However if subsequent cacheSolve call is made, then the cached
## value will be returned regardless of whether additional arguments are the
## same as with the initial call or not.
cacheSolve <- function(x, ...) {
        s <- x$getSolved()
        if (!is.null(s)) {
            message("Inverse matrix is taken from cache")
            return(s)
        }
        
        s <- solve(x$get(), ...)
        x$setSolved(s)
        s
}
