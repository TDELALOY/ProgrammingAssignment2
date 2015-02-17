## Thomas Delaloy
## 17.02.2015
##
## The function,  makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value , get the value , calculate the inverse and get the inverse

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


## The following function calculates the inverse of a "Matrix"
## it first checks to see if the inverse has already been calculated
## and store in memory via the function getinverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverted matrix")
                return(m)
        } else {
                m <- solve(x$get(), ...)
                x$setinverse(m)
                return(m)
	 }
}

#
# In order to validate the function, perform those commandes
#
# x <- matrix(c(1:4), 2, 2)  # should be inverted
# m <- makeCacheMatrix(x=x)
# cacheSolve(m)
# cacheSolve(m)

