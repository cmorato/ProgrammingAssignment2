# setwd("~/R/ProgrammingAssignment2")
# source("cachematrix.R")
#
# library(Matrix)
# x<-Matrix(rnorm(9), 3)
# x
# m = makeCacheMatrix(x)
# m$get()
# cacheSolve(m)
# cacheSolve(m)
#         getting cached data 

# makeCacheMatrix creates a special "matrix", which is really a list containing a function to

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) { 
    im <- NULL
    set <- function(y) {
      x <<- y
      im <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) im <<- inverse
    getinverse <- function() im
    list(set=set, 
        get=get, 
        setinverse=setinverse, 
        getinverse=getinverse)
}


# no singularity check - matrix is always invertible.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	  im <- x$getinverse()
    if(!is.null(im)) {
      message("getting cached data.")
      return(im)
    }
    data <- x$get()
    im <- solve(data)
    x$setinverse(im)
    im
}
