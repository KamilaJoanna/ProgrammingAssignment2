## Function 'makeCacheMatrix' creates a list containing a functions that cache matrix inverse. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){                           ## Function 'set' sets the value of matrix.
                x <<- y
                i <<- NULL
        }
        get <- function() x                           ## Function 'get' gets the value of matrix.
        setInverse <- function(inverse) i <<- inverse ## Function 'setInverse' sets the value of the inverse of matrix.
        getInverse <- function() i                    ## Function 'getInverse' gets the value of the inverse of matrix.
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Function 'cacheSolve' calculates the inverse of matrix represented by the list of functions
## created with the 'makeCacheMatrix' function.

cacheSolve <- function(x, ...) {
        i<- x$getInverse()
        if(!is.null(i)){       ##Checking if the inverse matrix has already been calculated.
                message("getting cached data")
                return(i)      ## If so, get the value of the inverse from the cache and don't calculate again. 
        }
        data<- x$get()         ## Otherwise, get the value of matrix from the cache
        i<- solve(data, ...)   ## and calculate the inverse of matrix.
        x$setInverse(i)        ## At the end, set the value of the inverse matrix in the cache
        i                      ## and return the inverse of matrix.
}