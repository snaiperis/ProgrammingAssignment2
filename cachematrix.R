## These functions implements matrix object capable to cache value
## of its inverse matrix. If calculation of inverse matrix (for the
## same source matrix) is done repeatedly, these function can reduce
## calculation time.
##
## Example: 
## 
## m <- matrix(c(2, 1:8), 3)
## system.time({ for( i in 1:100000 ) { solve(m) } })
## #   user  system elapsed 
## #  14.11    0.00   14.12 
## 
## mc <- makeCacheMatrix(m)
## system.time({ for( i in 1:100000 ) { cacheSolve(mc) } })
## #   user  system elapsed 
## #   0.95    0.00    0.95 



## makeCacheMatrix() function creates a matrix "object" capable
## to cache the inverse of the matrix
## Parameters:
##    x - matrix
## Returns
##    matrix "object" (actually R list)

makeCacheMatrix <- function(x = matrix()) {
        ## We will keep cached inverse matrix in variable 'inv'
        inv <- NULL
        set <- function(m) {
                ## '<<-' do assignment to parent environment variables above
                x <<- m
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(m) inv <<- m
        getInverse <- function() inv
        ## Let's return list of functions, which can set/get (local)
        ## variables of makeCacheMatrix() function (i.e. closure)
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve() calculates inverse of matrix and caches result. If 
## inverse matrix was calculated some time ago, returns cached value.
## Parameters:
##    x - matrix "object" created by makeCacheMatrix() 
## Returns
##    inverse of matrix passed by parameter

cacheSolve <- function(x, ...) {
        ## Let's try to take inverse matrix from cache
        inv <- x$getInverse()
        if( !is.null(inv) ) {
                ## Return cached inverse matrix
                return(inv)
        }
        ## Get data matrix and calculate inverse matrix
        m <- x$get()
        inv <- solve(m, ...)
        ## Store inverse matrix to cache
        x$setInverse(inv)
        ## Return (just calculated) inverse matrix
        inv
}
