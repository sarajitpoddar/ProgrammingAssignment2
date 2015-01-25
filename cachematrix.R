# |----------------------------------------------------------------------------|
# | SARAJIT PODDAR | 24 JAN 2015 SGT | COURSERA DATASCIENCE SUBMISSION
# |----------------------------------------------------------------------------|

# |----------------------------------------------------------------------------|
# PURPOSE OF THE FUNCTION
# |----------------------------------------------------------------------------|
# Matrix inversion is usually a costly computation and their may be some 
# benefit to caching the inverse of a matrix rather than compute it repeatedly 
# (there are also alternatives to matrix inversion that we will not 
# discuss here). Your assignment is to write a pair of functions that cache 
# the inverse of a matrix

# makeCacheMatrix: This function creates a special "matrix" object that 
# can cache its inverse.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse (of the matrix)
# 4. get the value of the inverse (of the matrix)
# |----------------------------------------------------------------------------|

makeCacheMatrix <- function(x = matrix()) {
        # initilizing the matrix variable m
        m <- NULL
        
        # defining function "set"
        # <<- operator is used to assign a value to an object in an 
        # environment that is different from the current environment. 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # defining function "get", "setinverse" and "getinverse"
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)

}


# Write a short comment describing this function
# The following function calculates the inverse of the matrix created with 
# the above function. However, it first checks to see if the inverse has already 
# been calculated. If so, it gets the inverse from the cache and skips 
# the computation. Otherwise, it calculates the inverse and sets the value 
# of the inverse in the cache via the setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data.")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}


## Sample run:
## > x = rbind(c(10, 20), c(30, 40))
## > m = makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,]   10   20
## [2,]   30   40
## No cache in the first run
## > cacheSolve(m)
## [,1]  [,2]
## [1,] -0.20  0.10
## [2,]  0.15 -0.05
## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
## [,1]  [,2]
## [1,] -0.20  0.10
## [2,]  0.15 -0.05
## > 