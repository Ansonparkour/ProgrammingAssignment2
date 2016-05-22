#function makeCacheMatrix creates a list containing a function to
# set: set the value of the matrix
# get: get the value from the matrix
# setinverse: set inverse of the matrix
# getinverse: get inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #get the x from input
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#function returns the inverse of the matrix.
#assume the matrix is always invertible.
cachinverse <- function(x, ...) {
    m <- x$getinverse()
    #checks if the inverse has already been computed.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    #computes the inverse and set it into the cache
    m <- solve(data, ...)
    x$setinverse(m)
    m
}