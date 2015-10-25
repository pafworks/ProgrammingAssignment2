## cachematrix.R
##
## Creates a matrix object which is cacheable and computes the inverse, if possible,
##   of that matrix.
##
##  Example usage:
##
## > mcm <- makeCacheMatrix(matrix(c(1.00,12,12,1.00), nrow=2,ncol=2))
## > cacheSolve(mcm)
## [,1]         [,2]
## [1,] -0.006993007  0.083916084
## [2,]  0.083916084 -0.006993007
##> cacheSolve(mcm)
## getting cached data
## [,1]         [,2]
## [1,] -0.006993007  0.083916084
## [2,]  0.083916084 -0.006993007
##
##
##
##
##

## The makeCacheMatrix function creates a special "matrix" object 
##  that can cache its inverse
##
## It also creates get/set functions for matrices, populates a
## list with those functions, and returns the list
##
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(get = get,
         set = set,
         setinv = setinv,
         getinv = getinv)

}


## The cacheSolve function computes the inverse (if possible) of the special "matrix" 
## returned by makeCacheMatrix above.
## 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
##
## Important Note: this function assumes that matrix 'x' can be inverted. If it cannot,
## the results are undetermined. Use at your own risk.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if (!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinv(m)
        m
}
