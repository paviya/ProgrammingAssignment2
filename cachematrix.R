## Assignment is to write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(mx = matrix()) {
    m <- NULL
    set <- function(y) {
        mx <<- y;
        m <<- NULL;
    }
    get <- function() return(mx);
    setinv <- function(inv) m <<- inv;
    getinv <- function() return(m);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))

}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

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
        ## Return a matrix that is the inverse of 'mx'

