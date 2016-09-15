## The below functions take a matrix and attempt to invert it.
## If the matrix has not already been inverted, then the
## inverted matrix will be generated and saved to the global environment.
## If the matrix has already been inverted, then the inverted matrix
## will be retrieved from the global environment.
## This provides a mechanism to save having to recalculate the inverted
## matrix.

## A short test harness for these functions is as follows:
## -------------------------------------------------------
## source("cachematrix.R")
## testMatrix<-matrix(c(4,2,7,6), nrow=2, ncol=2)
## testMatrix2<-makeCacheMatrix(testMatrix)
## invTestMatrix2<-cacheSolve(testMatrix2)
## invTestMatrix2
## invTestMatrix2<-cacheSolve(testMatrix2)
## -------------------------------------------------------
## The second last line above should result in the following matrix 
## being printed to the console
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## The last line should result in the following message being printed
## "getting cached data"


## The makeCacheMatrix function is passed a matrix and produces a 
## list containing functions that allow for storing and accessing 
## that matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(matrix) m <<- matrix
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function is passed the list produced by makeCacheMatrix
## and uses it to calculate the inverse of a matrix. If the inverse has  
## already been calcuated, then it will be retrieved from the global
## environment and a message "getting cached data" will be displayed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
