## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse
### makeCacheMatrix creates a list containing a function to
# 1. set the value of matrix
# 2. get the value of matrix
# 3. set the value of inversed matrix
# 4. get the value of inversed matrix

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set <- function(y) 
        {
            x <<- y
            inv <<- NULL
        }
    
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by
##makeCacheMatrix above
##Computing the inverse of a square matrix can be done with the solve 
##function in R. For example, if X is a square invertible matrix, 
##then solve(X) returns its inverse.
##For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) 
{
    inv <- x$getinverse()
    
    if(!is.null(inv)) 
        {
        message("Cached data is:")
        return(inv)
        }
    
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}
