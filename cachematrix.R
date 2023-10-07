## The function will calculate inverse of an input matrix 
## and then cache the resulted inverse of the matrix rather than
## compute it repeatedly


## This function will do the following to create a spectial matrix
## object that can cache its inverse:
## 1. Set the matrix
## 2. get the matrix
## 3. set the inverse of matrix
## 4. get the inverse of matrix 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function () x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set=set, get=get, setinv = setinv, getinv = getinv)
}


## This function will computes the inverse of the special matrix
## returned by makeCacheMatrix above.  If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve should retrive the inverse from 
## the cache.  Otherwise the cacheSolve should calculate the inverse of the 
## matrix. 

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
