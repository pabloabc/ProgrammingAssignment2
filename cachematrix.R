## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## This function creates a special "matrix" object that can cache its inverse.
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setMinv <- function(Minv) m <<- Minv
    getMinv <- function() m
    list(set = set, get = get,
         setMinv = setMinv,
         getMinv = getMinv)    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getMinv()
    if(!is.null(m)) {
        message(paste("Getting Matrix Inverse stored previously in cache.Using cached data.",
                      "Nothing computed here.", sep="") )
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setMinv(m)
    m    
}
