## These two functions together quicken processes
## which involve multiple computations of 
## inverse of a matrix

## makeCacheMatrix function helps in storing/retriving inverse matrix
## It also stores inverse matrix when it is calculated

makeCacheMatrix <- function(x = matrix()) {
        CacheInverse <- NULL
        set <- function(y) {
                x <<- y
                CacheInverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) CacheInverse <<- inverse
        getinverse <- function() CacheInverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## cacheSolve function checks if the matrix inverse is null or not
## and then returns actual inverse if null or returns cached value otherwise

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        Matrix <- x$get()
        inverse <- solve(Matrix, ...)
        x$setinverse(inverse)
        inverse
}

