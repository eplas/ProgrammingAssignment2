##  Pablo del Amor Saavedra 
##  makeCacheMatrix and cacheSolve are used to create a special "matrix" object, 
##  that allows its inversion to be cached, so after the first call, it doesn't need to
##  be computed again.  

####################################################################################################
##  This function creates a special "matrix" object, with 4 methods available:
##      -   set (initial matrix object)
##      -   get (returns matrix object)
##      -   setsolve (stores inverted matrix)
##      -   getsolve (returns stored inverted matrix)
##
##  *** Only required argument is a matrix object ***
####################################################################################################

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

####################################################################################################
##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##  if the computation is been done already, it gets the value stored in the object created
##  by makeCacheMatrix.
##  !!!!!! We assume that the matrix supplied is always invertible !!!!!
##
##  *** Only required argument is an invertible matrix created by the function makeCacheMatrix ***
####################################################################################################

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
