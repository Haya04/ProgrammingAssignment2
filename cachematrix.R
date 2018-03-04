## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(MT = matrix()) {
        inv <- NULL
        set <- function(y) {
                MT <<- y
                inv <<- NULL
        }
        get <- function() MT
        setinverse <- function(Inv_matrix) inv <<- Inv_matrix
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix()

cacheSolve <- function(MT, ...) {
        inverse <- MT$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- MT$get()
        inverse <- solve(,data, ...)
        MT$setinverse(inverse)
        MT
}