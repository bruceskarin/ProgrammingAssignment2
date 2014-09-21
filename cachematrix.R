## cacheMatrix.R contains functions for returning the inverse of a given square 
## invertable matrix.To improve response time, makeChacheMatrix will cache the
## inverse of a given matrix 'x' so that if the matrix remains unchanged,
## cacheSolve can save computation time and return the cached value.

## makeCacheMatrix creates a cached version of a given matrix, 'x'. setinverse
## and getinverse sub functions facilitate access of the cache matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns the inverse of a given matrix 'x'. If the inverse of 'x'
## as been previously calculated and the matrix is unchanged, then the cached
## value is returned. Otherwise, the inverse is solved.

cacheSolve <- function(x = matrix(), ...) {
        ## First see if there is a cached result
        m <- x$getinverse()
        ## If result is not null, then return cached value
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Otherwise get data solve for inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
        m
}

## Test evaluation:
#> test <- matrix(1:4,2,2)
#> m = makeCacheMatrix(test)
#> cacheSolve(m)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#> inverse <- cacheSolve(m)
# getting cached data
#> test%*%inverse
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1


