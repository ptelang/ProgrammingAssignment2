## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special matrix containing a list of functions to:
# set and get the matrix, set and get inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get, 
         setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# This function computes the inverse of the special matrxi. 
# It first checks to see if the inverse has already been calculated. 
# If so, it returns the inverse from the cache. Otherwise, it 
# computes the inverse of the matrix and sets it in the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

