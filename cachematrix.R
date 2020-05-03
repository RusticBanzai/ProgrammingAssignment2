## The following functions allow the user to create a special case of matrix
## that supports the calculation of the inverse of a matrix and cache the computed
## value to re-use without the need for a costly repeat of the computation

## makeCacheMatrix - creates a special "matrix", which is actually a list containing a function to
##  1) set the value of the matrix
##  2) get the value of the matrix
##  3) set the value of the inverse
##  4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    invisible(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
}


## cacheSolve - calculates the inverse of the special "matrix" created by makeCacheMatrix
##  -First checks to see if the inverse has previously been calculated and, if so, retrieves
##  the inverse from the cache and skips the computation.
##  -Otherwise, it calculates the inverse of the data and sets the value of the inverse in 
##  the cache via the setInverse function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    message("calculating matrix inverse")
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
