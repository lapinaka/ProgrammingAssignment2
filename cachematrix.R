## Assignment: Caching the Inverse of a Matrix.

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.  It sets the value of the matrix, gets
## the value of the matrix, sets the value of the inverse, gets the value
## of the inverse.

makeCacheMatrix <- function(X = matrix()) {
        inverse <- NULL
        set <- function(y) {
                X <<- y
                inverse <<- NULL                
        }
        get <- function() X
        setinverse <- function(inverse) inverse <<- inverse
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If X is a square invertible matrix,
## then solve(X) returns its inverse. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.
cacheSolve <- function(X, ...) {
        inverse <- X$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- X$get()
        inverse <- solve(data, ...)
        X$setinverse(inverse)
        ## Return a matrix that is the inverse of 'X'        
        inverse
}

## For example:
## X <- matrix(c(2,3,4,3,2,3,2,5,6,7,6,5,4,3,8,9),4,4)
## mat <- makeCacheMatrix(X)
## cacheSolve(mat)
