## The first function makes a special matrix
## able to cache its inverse. This can be helpful as solve() function
## is costly, therefore caching is beneficial - no unnecessary calculations.

## MakeCacheMatrix() function makes a special matrix
## able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- solve(x)
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## In case the inversed function to our special matrix has already been solved
## , we can access is via cacheSolve() function.

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
