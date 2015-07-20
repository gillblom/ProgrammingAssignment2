## Run a matrix you want to invert through makeCacheMatrix to create 
## a list of functions to call on the matrix, store the makeCacheMatrix
## in a variable. The function cacheSolve 
## will then make use of the functions in the variable list to calculate 
## the inverse when necessary, or just load the cached version.

## Take a matrix as input, create a list of functions 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## Check if inverse is cached,if so return cached inverse.
## If cache is null: compute inverse, save it into the cache and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    message("computing inverse and storing result in cache")
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}