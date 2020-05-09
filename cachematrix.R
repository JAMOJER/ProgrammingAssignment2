## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly (there are also alternatives to matrix 
## inversion that we will not discuss here). Your assignment is to 
## write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache 
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
## Manipulating the makeVector function we were to instead of 
## taking a vector and return its mean, taking a matrix
## and returning its inverse
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        
## Return a matrix that is the inverse of 'x'
## Manipulating the cachemean function and instead of caching
## means of vectors manipulating it to cache inverses of 
## vectors.
        
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- inverse(data, ...)
        x$setinverse(i)
        i
}
