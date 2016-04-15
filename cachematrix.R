## This function creates a special matrix that is actually a list containing
##   functions to:
##        1) set a matrix
##        2) get a matrix
##        3) invert the matrix
##        4) get the inverse of a matrix.

## Written by:  Matt Turner
## Date: April 12, 2016

makeCacheMatrix <- function(x = matrix()) {
    # Initialize the matrix to null
    m <- NULL
    
    # Create the set function
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    # Create the get function (it just returns x)
    get <- function(){
        x
    }
    
    # Create the inverse function
    storeInverse <- function(invertedMatrix){
        m <<- invertedMatrix
    }

    # Get the inverse matrix
    getInverse <- function(){
        m
    }
    
    list(set=set,get=get,storeInverse=storeInverse,getInverse=getInverse)
    
}

## The following function will take the inverse of a matrix.  If first checks
##   to see if the inverse has already been calculated.  If so, it reads
##   the inverse from cache.  If not, it calculates the inverse and stores
##   it in cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    # If the inverse is in cache, return the inverse and exit
    if(!is.null(m)){
        message("Getting the cached data...")
        return(m)
    }
    
    # Get the data
    data <- x$get()
    
    # Calculate the inverse of the data
    m <- solve(data)
    
    # Store the inverse of the data
    x$storeInverse(m)
    
    # Return the inverted matrix
    m
}
