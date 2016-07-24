## This is a pair of functions that cache the inverse of a matrix.


## The function "makeCacheMatrix" creates a "matrix" object (x) that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        cacheinverse <- NULL
        
## Set the value of x

        set <- function(y=matrix) {
                x <<- y
                cacheinverse <<- NULL
        }
## Get the value of x

        get <- function() x
        
## Set inverse value

        setinverse <- function(inverse) cacheinverse <<- inverse
        
## Get inverse value

        getinverse <- function() cachedinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The function "cacheSolve" retrieves the inverse from the cache if the inverse has already been calculated.
## If it has not been computed, "cacheSolve" calculates the inverse of the matrix x returned by "makeCacheMatrix". 

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        
        cacheinverse<-x&getinverse()
        
        ## Return inverse if it has already been calculated
        
        if(!is.null(cacheinverse)){
            message("getting inverse from the cache")
            return(cacheinverse)
        }
        
        ## Get matrix from object (x)
        
        data<-x&get()
        
        ## Compute the inverse of the matrix
        
        cacheinverse<-solve(data,...)
        
        ## Set the value of inverse
        
        x&setinverse(cacheinverse)
        cacheinverse
}

