## The two functions below are used to create and cache the inverse of a matrix.


## this first function creates a "special" matrix which is really a list containing a function to
        #1. set the matrix
        #2. get the matrix
        #3. set the value of the matrix inverse
        #4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y=matrix) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) mi <<- solve
        getinverse <- function() mi
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


### this function calculates the inverse of the "special" matrix created with the function above.
## However, it first checks to see if the inverse has already been calculated. If so, it gets the
# inverse from the cache and skips the computation.  Otherwise, it calculates the inverse
# of the matrix and sets the inverse of the matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mi <- x$getinverse()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data, ...)
        x$setinverse(mi)
        mi
}

