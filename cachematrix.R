## These functions will create a matrix object that will 
## have its inverse calcuated and cached

## This function creates/initializes the matrix
## and contains methods to set/get/setInverse/getInverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list (set = set, 
          get = get, 
          setInverse = setInverse,
          getInverse = getInverse)
}


## This function will solve the inverse of our special matrix
## and then if the data has not changed will cache the results

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
