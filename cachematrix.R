## The two functions here are to cache the computation of inversing a matrix,
## and the result can be retrieved for future use without recomputation. 
## Assume the matrix is always invertible.

## Create a special matrix that has a list of functions including to 
## get/set the matrix and its inversed matrix
makeCacheMatrix <- function(x = matrix()) {
    inversed <- NULL
    
    get <- function() x
    set <- function(m) {
        x <<- m
        inversed <<- NULL
    }  
    getInv <- function() inversed
    setInv <- function(inv) inversed <<- inv
    
    list(get = get, set = set, getInv = getInv, setInv = setInv)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    inversed <- x$getInv()
    if (is.null(inversed)) {
        message("calculate to inverse a matrix")
        m <- x$get()
        inversed <- solve(m, ...)
        x$setInv(inversed)
    }
    inversed
}
