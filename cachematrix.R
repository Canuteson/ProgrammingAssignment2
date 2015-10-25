## makeCacheMatrix sets is used to cache the inverse value of a matrix
## and provide lookups so the inverse matrix can be retrieved without recalculating

## creates a "matrix" which can store the inverse value

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function() {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## calculates the inverse matrix if not cached, or retrieves the 
## cached inverse if available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
