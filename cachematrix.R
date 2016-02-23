## Caching the Inverse of a Matrix

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inv_mat <<- inv
        getinverse <- function() { inv_mat }
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes inverse of the "matrix"

cacheSolve <- function(x, ...) {
        inv_mat <- x$getinverse()
        if(!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        data <- x$get()
        inv_mat <- solve(data, ...)     ## Return a matrix that is the inverse of 'x'
        x$setinverse(inv_mat)
        inv_mat
}
