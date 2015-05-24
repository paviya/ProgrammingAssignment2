## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mx = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mx);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))

}


## Write a short comment describing this function

cacheSolve <- function(mx, ...) {
    inverse <- mx$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- mx$get()
    invserse <- solve(data, ...)
    mx$setinv(inverse)
    return(inverse)
}
        ## Return a matrix that is the inverse of 'x'
}
