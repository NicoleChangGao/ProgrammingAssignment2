# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    # initialize the inverse matrix value
    inv <- NULL
    
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get the value of the matrix
    get <- function() x
    
    # set the value of the inverse
    setinverse <- function(inv_input) inv <<- inv_input
    # get the value of the inverse
    getinverse <- function() inv
    
    # return a list of all the above functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## The following function calculates the inverse of the special  "matrix" created with the above function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    # check whether the inverse is cached, if so, we can directly get the inverse from the cache 
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    # else, we get the matrix
    data <- x$get()
    # then we calculate the inverse
    inv <- solve(data, ...)
    # next, cache the inverse of the matrix
    x$setinverse(inv)
    # finally, get the result
    inv
}
