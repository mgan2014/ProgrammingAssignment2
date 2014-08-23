## This code is part of Programming Assignment 2 project for R Programming
## (https://class.coursera.org/rprog-006/)
## It provides a tool to speed up the evaluation of the inverse of a matrix
## by lookin up from a table which stores previously evaluated matrix.
## Computation expensive operations (inverse of matrix) is replaced by a
## faster looking up function

## 1/2 
## This is the first function which creates a "matrix" object by defining 
## its methods (set, get, setInverse, getInverse).

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    # set method: set the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # get method: get the matrix
    get <- function() x
    
    # setInverse method: save the inverse for future look-up
    setInverse <- function(inverse) i <<- inverse
    
    # getInverse method: retrieve previously calculated inverse
    getInverse <- function() i
    
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## 2/2
## This is the second function which calculates the inverse of the matrix
## if the previously calculated inverse non-exist, or retrive the stored
## inverse matrix if found.

cacheSolve <- function(x, ...) {

    # retrieving the inverse of the matrix
    i <- x$getInverse() 
    
    # if found, return stored inverse calculated before
    if(!is.null(i)) {   
        message("getting cached inverse matrix")
        return(i)
    }
    
    #if not found, calculate the inverse for the first time
    data <- x$get()
    i <- solve(data)    #calculate the inverse
    
    #save the calculated inverse for future use
    x$setInverse(i)
    
    ## Return a matrix 'i' that is the inverse of 'x'
    i
        
}
