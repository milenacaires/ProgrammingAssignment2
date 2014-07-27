## Two functions (makeCacheMatrix and cacheSolve) to be able to calculate 
## and cache inverse of a matrix which can pottentially be a time-consuming 
## computation. 
## For a small matrix, to calculate the inverse is typically a fast operation.
## However, for a big matrix, it may take too long to compute the inverse, 
## specially if it has to be computed repeatedly (e.g. in a loop). 
## If the contents of the matrix are not changing, it makes sense to cache 
## the value of the inverse so that when we need to again, it can be looked up
## in the cache rather than recomputed. 

## Function: makeCacheMatrix
## 
## This function creates a cache matrix object, i.e. 
## special "matrix", which is really a list containing a function to:
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse matrix
##      4. get the value of the inverse matrix
## 
## @param x: Matrix
## @result:  Cache matrix object

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## Function: cacheSolve
## 
## This function returns the inverse of the original matrix.
## If the inverse was already calculated, 
## it will return the cached inverse matrix.
## Otherwise, it will calculate the inverse matrix using the function 'solve', 
## cache it for future use, then return it.
## 
## @param x: Cache matrix object
## @result:  Inverse of the original matrix stored in the cache matrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
