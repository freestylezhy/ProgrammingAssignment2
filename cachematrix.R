## calculate the inverse of a matrix. The result is cached after the matrix 
## inverse is calculated.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse, 
## the matrix and its inverse can be accessed through set and get functions:
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix inverse
## get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    get <- function() x
    setinv <- function(inv) invx <<- inv
    getinv <- function() invx
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    invx <- x$getinv()
    if(!is.null(invx)) {
        message("getting cached data")
        return(invx)
    }
    data <- x$get()
    invx <- solve(data, ...)
    x$setinv(invx)
    invx
}
