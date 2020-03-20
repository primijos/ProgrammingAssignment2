## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# creates a "proxy object" for a Matrix, capable of caching
# some specific functions, in this case, the inverse
makeCacheMatrix <- function(x = matrix()) {
    # start state: cached val is not existing
    cached_val <- NULL
    # when a new matrix is assigned, keep it in this
    # proxy and invalidate previous cached value
    set <- function(y) {
        x <<- y
        cached_val <<- NULL
    }
    # return proxied matrix
    get <- function() {
        x
    }
    # keep this value as the cached value
    setinverse <- function(inv) {
        cached_val <<- inv
    }
    # return cached value
    getinverse <- function(){
        cached_val
    }
    # returns a list with accessors for the proxy functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# return a matrix that is the inverse of "x", looking
# first for cached computations, if none is found, it is
# computed and kept for the future
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # let's try to find the cached value...
    cached_val <- x$getinverse()
    if (is.null(cached_val)) {
        # if not found, we must compute it
        # get the proxied matrix
        tmp <- x$get()
        # compute the value
        inv <- solve(tmp,...)
        # keep it in the cache/proxy
        x$setinverse(inv)
        # and return it
        return(inv)
    } else {
        # cache hit! return that value
        return(cached_val)
    }
}
