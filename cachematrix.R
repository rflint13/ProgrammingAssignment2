## This file contains two functions, one function that creates a special matrix object
## which can cache its inverse and a second function that solves the inverse of
## the special matrix, leveraging the cached inverse if available.

## The makeCacheMatrix function creates a list object that stores a matrix and can 
## cache its inverse.  Expected input of this function is a invertable matrix.  
## The object has four functions, one to set the matrix (and clear the 
## inverse cache), a second to get the matrix, a third to set the inverse cache
## and a fourth to return the cached inverse.

makeCacheMatrix <- function(x= matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function takes an invertable matrix and returns its inverse
## The function takes as arguments a makeCacheMatrix object and any arguments that
## should be passed to the solve function.  The function will return the cached 
## inverse if available, and if not calculate the inverse, store it in the makeCacheMatrix
## object and return that inverse.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
