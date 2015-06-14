## makeCacheMatrix is a function
## As such, it's an object.
## For Object oriented developper, it can be seen as an instance of a class
## makeCacheMatrix returns actually the list of function which are getters / setters / procedures
## needed on matrix
## As required, there will be
##  - a getter to original matrix
##  - a setter to original matrix to reuse the special list managed by makeCacheMatrix
##    without recreating a new list
##  - a getter to a cache object which is the inverse of the original matrix


makeCacheMatrix <- function(x = matrix()) {
    ## x is the original matrix
    ## as a parameter of the function it's naturally cached
    
    ## inverse is the inverse of x
    ## as long as the getter on inverse is not called, inverse is not intialized
    inverse <- NULL
    ## inverse.tried is a cached value not to try and find an inverse if 
    ## it was done before and inverse doesn't exist
    inverse.tried <- FALSE
    
    ## setter of x
    setMatrix <- function (t) {
        x <<- t
        ## set a new matrix implies that the inverse is no more valid
        ## inverse and inverse.tried cached must be cleaned
        inverse <<- NULL
        inverse.tried <<- FALSE
    }
    ## getter of x
    getMatrix <- function() {
        x
    }
    
    # setter for inverse matrix and inverse.tried
    # It is not available in list of makeCacheMatrix (private setter)
    setInverse <- function (new.inverse, new.inverse.tried) {
        inverse <<- new.inverse
        inverse.tried <<- new.inverse.tried
    }
    
    # getter for inverse.tried
    # It is not available in list of makeCacheMatrix (private getter)
    getInverseTried <- function() {
        inverse.tried
    }
    
    # getter for inverse
    # It is available in list of makeCacheMatrix (public getter)
    # It's where the cache intellignece is
    getInverse <- function() {
        # If inversion has been tried
        if (getInverseTried()) {
            ## return inverse which has been initialized previously
            inverse
        } 
        ## sinon
        else { 
            ## save in cache the inverse and flag the attempt of calculation
            setInverse(try(solve(getMatrix())), TRUE)
            ## return inverse which has just been initialized
            inverse
        }
    }
    
    ## makeCacheMatrix returns the list
    ##  getter to original matrix
    ##  setter to original matrix
    ##  getter to cached inverse initializing cache when needed
    list(set = setMatrix, get = getMatrix, getInv = getInverse)
      
}


## cacheSolve return the inverse of a matrix set in makeCacheMatrix
## As the cache intelligence as already been implemented in public getter getInv
## cacheSolve just call this getter

cacheSolve <- function(x, ...) {
    ## Call getInv on x
    x$getInv()
}

## To be closer from the initial example, it could have been possible to use the following :

## makeCacheMatrixOld is a function
## As such, it's an object.
## We just use it as container for variables with getters and setters and no intelligence in them

makeCacheMatrixOld <- function(x = matrix()) {
    ## x is the original matrix
    ## as a parameter of the function it's naturally cached
    
    ## inverse is the inverse of x
    ## as long as the getter on inverse is not called, inverse is not intialized
    inverse <- NULL
    
    ## setter of x
    setMatrix <- function (t) {
        x <<- t
        ## set a new matrix implies that the inverse is no more valid
        ## inverse cached must be cleaned
        inverse <<- NULL
    }
    ## getter of x
    getMatrix <- function() { x }
    # setter for inverse matrix
    setInverse <- function (new.inverse) { inverse <<- new.inverse }
    # getter for inverse
    getInverse <- function() { inverse } 
    ## makeCacheMatrix returns the list
    ##  getter to original matrix
    ##  setter to original matrix
    ##  getter to cached inverse initializing cache when needed
    list(set = setMatrix, get = getMatrix, getInv = getInverse, setInv = setInverse)
    
}


## cacheSolveOld return the inverse of a matrix set in makeCacheMatrix
## CacheSolveOld has to deal with caching intelligence
cacheSolveOld <- function(x, ...) {
    # If inverse is not null (has been computed before), return it from cache
    cache <- x$getInv
    if (!is.null(x$getInv)) { cache }
    ## sinon
    else { 
        ## initialized cache local variable with computed inverse
        cache <- try(solve(getMatrix()))
        ## set new cache value
        x$setInv(cache)            
        ## return inverse which has just been initialized
        cache
    }
}
