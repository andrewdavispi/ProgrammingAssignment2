## The 2 functions below allow for the caching of the inverse of an
## invertible matrix. Matrix inversion can be a costly computation and so
## the ability to cache the inverse of a matrix can be beneficial in terms
## of performance.  The makeCacheMatrix function creates a special "matrix"
## object that can cache its inverse.  The cacheSolve function computes the
## inverse of the special "matrix" object created by the makeCacheMatrix
## function.  The 2 functions take advantage of the scoping rules of R and
## how they can be manipulated to preserve state inside of an R object.


 
## The makeCacheMatrix function creates a special "matrix" object which
## can cache its inverse. The special "matrix" object consists of a list
## of 4 "set" and "get" functions:
##    1. a set function for the matrix
##    2. a get function for the matrix
##    3. a set function for the inverse of the matrix
##    4. a get function for the inverse of the matrix
## These 4 functions coupled with some internal variables and the scoping
## rules of R enable the caching of the inverse matrix.
## The input parameter x is a matrix.

makeCacheMatrix <- function(x = matrix()) { 
    
    minv <- NULL
    
    ## a funtion to set the "value" of the matrix and to set the inverse 
    ## of the matrix to NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    
    ## a function to get the matrix; it returns the matrix
    get <- function() x
    
    ## a function to set the value of the inverse of the matrix; note that
    ## this function does not actually calculate the inverse of the matrix;
    ## the actual inverse calculation occurs in the cacheSolve function
    setinv <- function(inv) minv <<- inv
    
    ## a function to get the inverse of the matrix; it returns the inverse
    ## of the matrix if the inverse has already been calculated; it returns
    ## Null if the inverse of the matrix has not already been calculated
    getinv <- function() minv
    
    ## return a list of the 4 functions above
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)

} 
 
 

## The cacheSolve function computes the inverse of the special "matrix"
## created by the makeCacheMatrix function.  The cacheSolve function first
## checks to see if the inverse has already been calculated and stored in
## the cache.  If so, it skips the inverse computation and returns the 
## cached inverse matrix directly.  Otherwise, it will calculate the
## inverse using the solve function, cache the inverse using the setinv
## function, and then return the inverse.
## The input parameter x is a makeCacheMatrix object.

cacheSolve <- function(x, ...) { 
   
    ## get the inverse of the matrix from the cache using the getinv
    ## function of the makeCacheMatrix object (x)
    minv <- x$getinv()
    
    ## if the retrieved inverse is not Null, then return the retrieved
    ## inverse from the cache
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    
    ## otherwise, get the matrix, compute its inverse using the solve
    ## function, set/cache the inverse using the setinv function of the 
    ## makeCacheMatrix object (x), and then return the inverse
    xmat <- x$get()
    minv <- solve(xmat, ...)
    x$setinv(minv)
    minv

} 
