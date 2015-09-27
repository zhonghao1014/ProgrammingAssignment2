
## Solving the inverse of a square matrix could be time-consuming for very large matrices. 

## In the cases where a matrix's inverse has to be computed repeatedly, it make sense that
## we could cache the inverse of matrix in itself so that when the content of the matrix 
## is not changed, previously calculated inverse could be used.

## makeCacheMatrix: creator of cache matrix that can cache its inverse
## cacheSolve: computes the inverse of the special "matrix" returned by 
##              makeCacheMatrix above.

## makeCacheMatrix: This function creates a special "matrix" object that can cache 
## its inverse. The special "matrix" object is like a matrix with functions to
## set/get the containing matrix and functions to set/get the inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) m <<- inv
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}

## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}