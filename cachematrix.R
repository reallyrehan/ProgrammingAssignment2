## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its inverse for the input (the input is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
        
        invM <- NULL
        
        set <- function(y) {
                x <<- y
                invM <<- NULL
        }
        
        get <- function() x
        
        setinvMerse <- function(invMerse) invM <<- invMerse
        
        getinvMerse <- function() invM
        
        list(set = set, get = get, setinvMerse = setinvMerse, getinvMerse = getinvMerse)
}


## this function can calculate the inverse of a given square matrix. It is returned by the above function if the inverse has already been calculated and if the matrix hasn't changed, this function will automatically retrieve the cache-d result

cacheSolve <- function(x, ...) {
        
        invM <- x$getinvMerse()
        
        if(!is.null(invM)) {
                message("getting result")
                return(invM)
        }
        
        data <- x$get()
        
        invM <- solve(data, ...)
        
        x$setinvMerse(invM)
        
        invM
}

