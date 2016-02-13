## This is my input for Coursera Assignment 2
## We are going to store a matrix
## In order to do that we will make a function
## called makeCacheMatrix.  THis will be a shell
## to store a matrix in

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## We now have a stored matrix

## Below we are going to calculate the inverse of our matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getmatrix()
          
          ## Now we want to check, if we already calculated
          ## the inverse of that matrix we will call the msg
          ## "getting cached matrix" and just return our stored inverse
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
## here's an example
## example
## P <- matrix (c(1:4), nrow = 2, ncol = 2)
##  P
##        [,1] [,2]
##  [1,]    1    3
##  [2,]    2    4

## > JJ <- makeCacheMatrix(P)

## > cachesolve(JJ)
##        [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5

##  > R<- makeCacheMatrix(P)
##  > cachesolve(R)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cachesolve(JJ)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cachesolve(R)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
