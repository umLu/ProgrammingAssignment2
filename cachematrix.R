## The function below creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## Set a initial NULL value to the variable that store inversions
    set <- function(y) {
        x <<- y ## If someone set another matrix, replace the old one
        inv <<- NULL ## And set to NULL an old result from a inversion
    }
    get <- function() {
        x ## Return the matrix that was set
    }
    setinv <- function(inverse) {
        inv <<- inverse ## Function used in cacheSolve to store a new inv
    }
    getinv <- function() {
        inv ## Store inversion, calculated by cacheSolve
    }
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function below computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.

cacheSolve<- function(x, ...) {
    inv <- x$getinv() ## Checks cache 
    if(!is.null(inv)) { ## If diferent than NULL
        message("Getting cached data")
        return(inv) ## Return the calculated inverse from cache
    }
    data <- x$get() ## Otherwise get the values
    inv <- solve(data, ...) ## And calculate the inverse
    x$setinv(inv) ## Setting the result to the special matrix cache
    inv ## Return a matrix that is the inverse of 'x'
}