## The following functions work together to create a square invertible matrix
## and make the inverse of the matrix available in the cache environment
##
## Sample run-time example included results
## > source("cachematrix.R")    load R program
## > a <- makeCacheMatrix()     create functions
## > a$set(matrix(1:4, 2, 2))   create matrix in working environment
## > cacheSolve(a)              1st run returns inverted matrix
##                              from working environment
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > cacheSolve(a)              2nd and subsequent runs
##                              returns inverted matrix from cache
## getting cached data          
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

makeCacheMatrix <- function(x = matrix()) {
    # stores the cached value
    # initialize to NULL
    m <- NULL
    
    set <- function(y) {
        # create the matrix in the working environment
        x <<- y
        m <<- NULL
    }
    
    # get the value of the matrix
    get <- function() x
    
    # invert the matrix and store in cache
    setinverse <- function(solve) m <<- solve
    
    # get the inverted matrix from cache
    getinverse <- function() m
    
    # return the created functions to the working environment
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it it created in the working environment and it's inverted value
## is stored in cache
cacheSolve <- function(x, ...) {
    ## attempt to get the inverse of the matrix stored in cache
    m <- x$getinverse()
    
    # return inverted matrix from cache if it exists
    # else create the matrix in working environment
    if(!is.null(m)) {
        message("getting cached data")
        # display matrix in console
        return(m)
    }
    
    # create matrix 
    data <- x$get()
    
    # set and return inverse of matrix
    m <- solve(data, ...)
    
    # set inverted matrix in cache
    x$setinverse(m)
    
    # display matrix in console
    m
}