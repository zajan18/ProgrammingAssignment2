## This whole code can be used for computing the inverse of a square matrix.
## There are two functions called "makeCacheMatrix" and "cacheSolve".
## makeCacheMatrix is used for get the value of matrix and inverse as well as set the value of matrix and inverse.
## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.

## This function contains get, set, getInverse, setInverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## computes the inverse of the rturned matrix from obove function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}