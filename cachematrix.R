## This file contains two functions which together serve to calculate the
## inverse of an invertible matrix and cache it for further use. The cached
## value can then be accessed instead of performing the calculation again. The
## cacheSolve function automatically determines if the calculation is necessary
## by checking whether a cached value exists.

## makeCacheMatrix returns an object which can store the values of a matrix
## and its inverse and access the stored values. (This object is techically
## a list of four functions which do the storing and accessing, and the cached
## matrix information is stored in variables local to these functions.) The
## inverse can be calculated and stored by the associated function cacheSolve.
## makeCacheMatrix takes an invertible matrix as its argument, or will create
## an empty one if no argument is given.

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        ## set the value of the inverse to NULL, as it has not yet been cached
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        ## set the value of the matrix to be stored, and reset its cached
        ## inverse to NULL
        get <- function() x
        ## return the value of the stored matrix
        setinverse <- function(inverse) I <<- inverse
        ## set the value of the matrix's inverse (this function is only to
        ## be called by cacheSolve after the inverse has been calculated)
        getinverse <- function() I
        ## return the value of the cached inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        ## return the four functions defined above as a list
}

## cacheSolve takes as its argument the object returned by calling
## makeCacheMatrix. It determines whether the inverse of the matrix is
## already cached in that object, and calculates and caches it if not.

cacheSolve <- function(x, ...) {
        I <- x$getinverse()
        ## check for a cached inverse matrix by calling getinverse
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        ## if there is a cached inverse, return it and print a message
        ## indicating that the previously cached value has been used
        data = x$get()
        ## if the inverse has not been cached, get the stored matrix from
        ## x by calling get
        I <- solve(data, ...)
        ## calculate the inverse of the matrix using the built-in "solve"
        ## function
        x$setinverse(I)
        ## cache the resulting inverse matrix in object 'x'
        I
        ## Return a matrix that is the inverse of the one stored in 'x'
}

