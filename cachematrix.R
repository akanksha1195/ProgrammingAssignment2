## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a vector which cache the inverse of matrix
# Four inner functions are created in this function namely :
# 1. set()
# 2. get()
# 3. setinverse()
# 4. getinverse()
makeCacheMatrix <- function(x = matrix()) {
    # set inv as NULL
    inv <- NULL

    # Assigning new argument to stored value
    # reset inv to NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    # return object
    get <- function() x

    # setinverse() is used to cache the value of inverse matrix
    setinverse <- function(inverse) inv <<- inverse

    # getinverse() is used to get the value of cached inverse matrix
    getinverse <- function() inv
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

# The following function is used to calculate inverse of sqaure matrix
# returned by makeCacheMatrix object. If the inverse was pre-calculated
# and matrix was not changed, then cacheSolve gets value from cache
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()

    # Check if original matrix was modified
    if(!is.null(inv)) {
        message("getting cached data")
        # return cached data
        return(inv)
    }
    data <- x$get()

    # Calculate inverse
    inv <- solve(data, ...)

    # set the inverse
    x$setinverse(inv)

    # return inverse matrix
    inv
}
