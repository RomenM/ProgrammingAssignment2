## makeCacheMatrix: creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve: computes inverse of the special "matrix" returned by makeCacheMatrix
## If inverse is already cached, returns cached version.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)   # compute inverse
    x$setInverse(inv)         # cache it
    inv
}
