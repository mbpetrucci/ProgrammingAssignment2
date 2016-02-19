## The following functions are to accomplish the inversing of a Matrix.
## Cashing an inverted matrix is useful when you want to use that inverted matrix repeatedly.
## Since it is a costly operation, you don't want to invert it every time you need to use it - provided the matrix does not change.

##Below are two functions.  These functions are used to invert a matrix and cache the result.

makeCacheMatrix <- function(x = matrix()) {

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function retrieves the cached matrix, if it exists.  If it doesn't exist,
## the matrix is retrieved and then inversed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
