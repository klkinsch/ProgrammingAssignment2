##  A set of functions that:
##  1. makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
##  2. cacheSolve:  Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##      If the inverse has already been calculated (and the matrix has not changed), 
##      then the cachesolve retrieves the inverse from the cache.
##  For this assignment, it is assumed that the matrix supplied is always invertible.

##  makeCacheMatrix:  
##    input is a square invertible matrix
##    returns a list containing get and set functions for the matrix and inverse.
makeCacheMatrix <- function(x = matrix()) {
    im = NULL
    set = function(y) {
        x <<- y
        im <<- NULL
    }
    get = function() x
    setinv = function(inverse) im <<- inverse 
    getinv = function() im
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


##  cacheSolve:
##  input is output of makeCacheMatrix
##  returns inverse of matrix input to makeCacheMatrix
cacheSolve <- function(x, ...) {
    im = x$getinv()
    if (!is.null(im)){
        message("getting cached data")
        return(im)
    }
    y.data = x$get()
    im = solve(y.data, ...)
  
    x$setinv(im)
    return(im)
}