## Function 'makeCacheMatrix' creates a list of 4 functions:
## set - to set the value of the matrix x (and reset value of inverse matrix)
## get - to get the value of the matrix x from the cache
## setinverse - to calculate and set the value of inverse matrix, using solve()
## getinverse - to get the value of the inverse matrix from the cache.

## Values of original matrix and inverse matrix are kept in the cached 
## environment, common for all 4 functions (environment where they were defined)

makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL
        set <- function(y) {
                x <<- y
                invx <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) invx <<- solve
        getinverse <- function() invx
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function 'cacheSolve' calculates the inverse of the matrix provided to 
## previous function 'makeCacheMatrix'.
## The argument x is a list of functions created by 'makeCacheMatrix'.
## First, 'cacheSolve' checks if the inverse matrix has already been calculated.
## If so, it returns the inverse matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse matrix and sets its value in the cache 
## via the 'setinverse' function.

cacheSolve <- function(x, ...) {
        invx <- x$getinverse()
        if(!is.null(invx)) {
                message("getting cached data")
                return(invx)
        }
        data <- x$get()
        invx <- solve(data, ...)
        x$setinverse(invx)
        invx
}

## To use these functions, save the result of 'makeCacheMatrix' (a list) 
## in a variable, e.g. FunList. You can obtain inverse matrix by using 
## 'cachSolve' on that FunList. If you need to change the input matrix,
## use FunList$set(), this will also reset the inverse value to NULL 
## and cachSolve(FunList) will have to calculate and return new inverse value.