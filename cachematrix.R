## Calculating the inverse of a matrix with caching.

#' Creates a special "matrix" object that can cache its inverse.
#'
#' @param x A matrix.
#' @return The Inverse of matrix x.
#' @examples
#' makeCacheMatrix(x)
makeCacheMatrix <- function(x = matrix()) {

    # Initialize.
    inverse <- NULL
    # Set and getMatrix.
    setMatrix <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    getMatrix <- function() x

    # Set and getInverse.
    setInverse <- function(solveInverse) inverse <<- solveInverse
    getInverse <- function() inverse

    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}

#' Computes the inverse of the special "matrix" returned by
#' makeCacheMatrix. If the inverse has already been calculated (and
#' the matrix has not changed), then cacheSolve retrieves the inverse
#' from the cache.
#'
#' @param x A matrix.
#' @return The Inverse of matrix x.
#' @examples
#' cacheSolve(x)
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("Getting cached data.")
        return(inverse)
    }
    inverse <- solve(x$getMatrix())
    x$setInverse(inverse)
    inverse
}

## # testCacheMatrix.
## # example from http://www.mathwords.com/i/inverse_of_a_matrix.htm

## mat <- matrix(data = c(4,3,3,2), nrow = 2, ncol = 2)
## mat2 <- makeCacheMatrix(mat)
## cacheSolve(mat2)
## cacheSolve(mat2)
