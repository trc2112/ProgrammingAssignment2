## Put comments here that give an overall description of what your
## functions do

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(newMatrix) {
        x <<- newMatrix
        inverse <<- NULL
    }
    get <- function() x
    getInverse <- function() inverse
    setInverse <- function(newInverse) inverse <<- newInverse
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


#This function computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated(and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        return(inverse)
    }
    matrixToBeInversed <- x$get()
    inverse <- solve(matrixToBeInversed, ...)
    x$setInverse(inverse)
    inverse
}
