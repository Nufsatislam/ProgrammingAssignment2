## Put comments here that give an overall description of what your functions do:
## Inverse of a vector is a vector with same magnitude but different direction. 
## The same thing goes with inverse matrix. its a set of vector which has magnitude with opposite directions. or in this case negative.
## the function given below can cache its inverse through getting and setting its value.
## on the other hand, the second function checks and returns an inverse matrix which will be produced by the first function.

## Write a short comment describing this function:
## The function makecachematrix creates a special "vector" which can cache its invers in the later part.
## this is a list containing a function to set the value of the vector,  get the value of the vector, set the value of the vector and get the value of the inverse vector.


makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL                           ## setting the value of the inverse vector
        set <- function(y){
                x <<- y
                invrs <<- NULL
}
        get <- function() x                     ## gets the value of the inverse vector
        setInverse <- function(inverse) invrs <<- inverse               ## setting the value of the inverse vector.
        getInverse <- function()invrs                                   ## getting the value of the inverse vector.
        list (set = set,
              get = get,
              setInverse = setInverse,
              getInverse = getInverse)
}

## Write a short comment describing this function
## The function below calculates the mean of the special "vector" created with the above function. 
## it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.
## The function cacheSolve should retrieve the inverse from the cache, If the inverse has already been calculated (and the matrix has not changed).


cacheSolve <- function(x, ...) {                        ## This function given below, computes the inverse of the special "matrix" returned by makeCacheMatrix. 
        
        invrs <- x$getInverse()         
        if (!is.null(invrs)){
                message("getting cached data")
                return(invrs)                           ## the matrix is always invertible.
}
        invmatrx <- x$get()
        invrs <- solve(invmatrx, ...)
        X$setInverse(invrs)
        invrs                                           ## Returns a matrix which is inverse of a given matrix 'x'. this 'x' matrix is set by the function above.
}
