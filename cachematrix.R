## Author: Venketraman Ganesan

# This function creates a special matrix object
# that can cache its inverse. This method takes a
# matrix which is assumed to be invertible as input parameter
# and return list of methods that contains setter and getter 
# methods for the input matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    # set the inverse of x as NULL
    xinv <- NULL
    
    # setter method using which the input matrix can be changed
    setmatrix <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    #getter method that returns input matrix
    getmatrix <- function() x
    
    #setter method to set the value of inverse matrix
    setinverse <- function(inverse) xinv <<- inverse
    
    #getter method to get the value of inverse matrix
    getinverse <- function() xinv
    
    #list of methods are returned
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}


#This function computes the inverse of the special matrix object
#created by makeCacheMatrix method.  If the matrix hasn't changed
#and inverse has been already calculated, the this function
#retrieves the inverse from the cache.  This method takes a special
#matrix object created by makeCacheMatrix method as input and return
#the inverse of that matrix.

cacheSolve <- function(x, ...) {
    
    #get the inverse matrix
    x_inv <- x$getinverse()
    
    #if the inverse matrix has been already calculated
    #return the inverse matrix from cache
    if(!is.null(x_inv)) {
        message("getting inverse matrix from cache")
        return(x_inv)
    }
    
    #the following code is run when the inverse matrix is not
    #already calculated.  This happens when
    #(1) when this method is run first time
    #(2) when the input matrix is changed using setter method
    # to compute the inverse of another invertible matrix
    
    #get the input matrix
    data <- x$getmatrix()
    
    #compute the inverse of the input matrix
    x_inv <- solve(data)
    
    #set the inverse matrix so that it can be retrieved from
    #cache in subsequent runs
    x$setinverse(x_inv)
    
    #return inverse matrix
    x_inv
}
