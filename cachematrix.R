#The following functions create and store a matrix object that can cache and
#retrieve its inverse. If a given matrix's inverse has not been computed and
#cached already, then the inverse will be calculated and stored.

#Create a special matrix object that can cache its inverse for easy computation
makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invrs <<- inverse
        getinverse  <- function() invrs
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#Compute the inverse of the special "matrix" returned by makeCacheMatrix.
#If the inverse has already been calculated, it will be retrieved from cache.
cacheSolve <- function(x, ...) {
        invrs <- x$getinverse()
        if(!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
        mat <- x$get()
        invrs <- solve(mat, ...)
        x$setinverse(invrs)
        invrs
}

#Testing function -----------------------------------------
#Note - not required for assignment
m <- matrix(rnorm(16),4,4)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)

n <- matrix(1:4, 2, 2)
n1 <- makeCacheMatrix(n)
cacheSolve(n1)