## Pair of functions to extablish methods to:
## cache an index and its inverse;
## recover the cached matrix and/or inverse

## takes a matrix x and returns a list of four functions: 
##  makeCacheMatrix$set to cache x;
##  makeCacheMatrix$get to return the cached x;
##  makeCacheMatrix$setinverse to cache the inverse of x;
##  makeCahceMatrix$getinverse to return the inverse of x

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## takes the list created by makeCacheMatrix, calcuates and sets the inverse
## of the matrix x cached by makeCacheMatrix$set, caches the inverse using 
## makeCacheMatrix$setinverse and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
