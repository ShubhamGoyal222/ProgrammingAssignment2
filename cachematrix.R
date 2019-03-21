## This function creates a special matrix object that can cache its inverse
## It's a list which can set the values of matrix, get its values, 
## set the values of inverse, get the inverse values

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function calculates the inverse of the special matrix created above
## If the inverse of the same matrix is already calculated, it gets the 
## inverse from the cache

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(inv)
    inv
}