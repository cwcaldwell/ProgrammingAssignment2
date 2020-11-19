## Caches the inverse of a matrix rather than computing it
## repeatedly. Assumes matrix supplied is invertible.

## Creates a special "matrix" object that can cache ts inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function (solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Calculates and caches the inverse of the special "matrix"
## if not previously cached (and the matrix is unchanged).
## Otherwise, returns the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
            message('getting cached data')
            return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv
}
