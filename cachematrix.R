
## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse. If X is a square invertible matrix,
## then solve(X) returns its inverse.
makeCacheMatrix <- function(X = matrix()) {
        m1 <- NULL
        set1 <- function(y1) {
                X <<- y1
                m1 <<- NULL                
        }
        get1 <- function() X
        setsolve <- function(solve) m1 <<- solve
        getsolve <- function() m1
        list(set1 = set1, get1 = get1,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.
cacheSolve <- function(X, ...) {
        m1 <- X$getsolve()
        if(!is.null(m1)) {
                message("getting cached data")
                return(m1)
        }
        data1 <- X$get1()
        m1 <- solve(data1, ...)
        X$setsolve(m1)
        ## Return a matrix that is the inverse of 'X'        
        m1
}

## For example:
## X <- matrix(c(2,3,4,3,2,3,2,5,6,7,6,5,4,3,8,9),4,4)
## mat1 <- makeCacheMatrix(X)
## cacheSolve(mat1)
