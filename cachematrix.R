## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
## ---------------------------------------------------------------------
## ---------------------------------------------------------------------
## Example usage:
## x <- matrix(rnorm(9), nrow = 3)      -- to create matrix with random numbers
## special_matrix <- makeCacheMatrix(x) -- creating "special" matrix
## special_matrix$get()                 -- returning "special" matrix
## cacheSolve(special_matrix)           -- calculation of inverse matrix
## cacheSolve(special_matrix)           -- returning cached inverse matrix
## ---------------------------------------------------------------------
## ---------------------------------------------------------------------
## 'makeCacheMatrix' function is used to create a special object
## that stores a square invertible matrix and cache's its inverse matrix
## it also creates a special "vector", which is really a list containing
## a function to:
## 1) set the value of the invertible matrix -->'set'
## 2) get the value of the invertible matrix --> 'get'
## 3) set the value of the inverse matrix --> 'setInverse'
## 4) get the value of the inverse matrix --> 'getInverse'
## ---------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
        ## empty object for cached inverse matrix
        inverse_matrix <- NULL
        ## setting "special" matrix
        set <- function(new_matrix) {
                x <<- new_matrix
                inverse_matrix <<- NULL
        }
        ## getting / returning "special" matrix
        get <- function() x
        ## setting up inverse matrix
        setInverse <- function(inversed) inverse_matrix <<- inversed
        ## getting inverse matrix
        getInverse <- function() inverse_matrix
        ## returning matrix of functions defined for matrices
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## ---------------------------------------------------------------------
## Write a short comment describing this function
## ---------------------------------------------------------------------
## ---------------------------------------------------------------------
## to have an Inverse, the matrix must be "square",
## which means it needs to have the same number of rows and columns
## and also the determinant cannot be zero
## The following function calculates the inverse matrix of the special "matrix"
## created with the 'makeCacheMatrix' function.
## It first checks to see if the inverse matrix has already been calculated and
## if the original matrix has not changed.
## If so, it gets the inverse matrix from the cache and skips the computation.
## Otherwise, it calculates the new value for the inverse matrix of the
## matrix provided and sets it in the cache via the setInverse function.
## ---------------------------------------------------------------------

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## loads what we have for inverse matrix
        inverse_matrix <- x$getInverse()
        ##  if the inverse has already been calculated
        ## and the original matrix has not changed
        ## inverse matrix gets loaded from cache
        if(!is.null(inverse_matrix)) {
                message("... getting inversed matrix from cache ...")
                return(inverse_matrix)
        }
        ## otherwise new matrix gets loaded
        matrix_data <- x$get()
        ## and inverse matrix is calculated with solve() function
        inverse_matrix <- solve(matrix_data, ...)
        ## new inverse matrix value is stored in the cache via setInverse
        x$setInverse(inverse_matrix)
        ## and new inverse matrix is returned from function
        inverse_matrix
}