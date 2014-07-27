## Matrix inversion can be a costly computation so there is some benefit to caching the inverse of a matrix.
## These functions create a matrix object capable of caching it's inverse, and the second computes the inverse (or calls the cached version if the matrix has not changed).


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        iM <- NULL
        set <- function(y) {
                x <<- y
                iM <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) iM <<- solve
        getInverse <- function() iM
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
		 iM <- x$getInverse()
	if(!is.null(iM)) {
			message("getting cached data")
			return(iM)
	}
	data <- x$get()
	iM <- solve(data, ...)
	x$setInverse(iM)
	iM
}
