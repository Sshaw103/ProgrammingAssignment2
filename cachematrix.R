## This set of functions takes a square matrix (that is invertible) 
##and computes and caches its inverse. Future runs will return
## the cached value, rather than recomputing it.
## A good test is a <- makeCacheMatrix(matrix(1:4,2)), which should
## return       [,1] [,2]
##        [1,]   -2  1.5
##        [2,]    1 -0.5

## makeCacheMatrix creates a list of four functions that will 
##operate on a vector x, which is expressed as a square matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## This function takes the special matrix given by makeCacheMatrix() 
##and returns the inverse of this matrix. 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}