## R file Description
# The functions in this R file build on the internal 'solve' function
# for calculating the inverse of a matrix by allowing for values to be
# cached for future use. 


## makeCacheMatrix Description
# Returns a special vector with functions to manipulate the matrix 
# and can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve Description
# Takes a 'special vector' of the type returned by makeCacheMatrix and
# returns the inverse of the matrix. If available, this is retrieved from
# cache, otherwise it is calculated and set to cache.

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
