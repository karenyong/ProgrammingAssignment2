## Functions that cache the inverse of a matrix

## Create a special "matrix", which is a list containing a function to:
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
     s <- NULL
     set <- function (y) {
          x <<- y
          s <<- NULL
     }
     
     get <- function() x
     setinverse <- function(inverse) s <<- inverse
     getinverse <- function() s
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     s <- x$getinverse()
     if(!is.null(s)) {
          message ("getting cached data")
          return (s)
     }
     
     data <- x$get()
     s <- solve(data, ...)
     x$setinverse(s)
     s

}

