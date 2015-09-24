## Create a list of functions that can get and set the inverse of a matrix. 
## Compute the inverse of the matrix object.
## If inverse has already been calculated and matrix is unchanged,
## return the inverse from the cache.
## Assume matrix supplied is always invertible.

## First create a matrix object that can cache its inverse
## makecacheMatrix creates a list of functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## initialize inv to NULL
        set <- function(y) {
                x <<- y
                ## subtitute x with input y in main function
                inv <<- NULL
                ## restore value of inv to NULL
        }
        get <- function () x
        setinverse <- function (inverse) inv <<- inverse
        ## set the value of inv to inverse
        getinverse <- function () inv
        ## get the value inv
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        ## store four functions in makeCacheMatrix
        
}


## Compute inverse of matrix from makeCacheMatrix.
## If inverse already computed, retrieve from cache.
## Otherwise calculate the inverse.
## Return a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        ## for inverse already calculated
        if(!is.null(inv)){
                message("getting cached data")
                ## output notice that cached data is being retrieve
                return(inv)
                ## return cached inverse
        }
        data <- x$get()
        inv <- solve(data, ...)
        ## otherwise, calculate inverse of the matrix
        x$setinverse(inv)
        ## set inverse in the cache
        return(inv)
        ## return calculated inverse
        
}
