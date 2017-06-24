## The Below Functions Help in cache the inverse of a matrix

## makeCacheMatrix : creates a special "matrix" object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
        ## initialize the cache property
        i <- NULL
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        ## set the value of the inverse
        setmean <- function(inverse) i <<- inverse
        ## get the value of the inverse
        getmean <- function() i
        ## Return a list of methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve : computes the inverse of the "matrix"
## If the inverse has already been calculated (and the matrix has not been changed)
## then cacheSolve should retrive the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        ## Return the inverse if it is already set
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## get the value of the matrix from x
        data <- x$get()
        ## Calculate the value of the inverse using matrix multiplication
        i <- solve(data) %*% data
        ## set the value of the inverse
        x$setmean(i)
        ## Return the value of the Inverse matrix
        i
}