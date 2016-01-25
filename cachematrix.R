## Functions will let the user to calculate the inverse of a matrix and cache 
## it if there is no cached inverse of the matrix exist. 
## If there is a cached inverse for the matrix, it will be returned instead.


## makeCacheMatrix() function returns a list of functions:
## set(), get(), setinverse() and getinverse(). 
## set()        - sets the value of the matrix
## get()        - gets the value of the matrix
## setinverse() - sets the value of the inverse
## getinverse() - gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL 
    set <- function(y){ 
        x <<- y         
        inverse <<- NULL    
    }
    get <- function() x  
    setinverse <- function(inv) inverse <<- inv 
    getinverse <- function() inverse 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}

## cacheSolve() function calculates and returns the inverse of a matrix or 
## returns the cached value of inverse, if cache is not empty.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse() 
    if(!is.null(inverse)){
        message("Getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data,...)
    x$setinverse(inverse)
    inverse
}

