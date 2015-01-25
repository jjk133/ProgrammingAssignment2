## These functions are designed to cache the inverse of a matrix

## makeCacheMatrix - Makes a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setm <- function(solve) m <<- solve
    getm <- function() m
    list(set = set, get = get,
         setm = setm,
         getm = getm)
    
}


## cacheSolve - Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    m <- x$getm()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    mdata <- x$get()
    m <- solve(mdata, ...)
    x$setm(m)
    m
}
