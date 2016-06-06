

##This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        
        ##Calculate the inverse matrix
        get <- function() x
        setInverse <- function(inverse) inver <<- inverse
        getInverse <- function() inver
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the matrix created. If the inverse has already been calculated then it should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getInverse()
        
        ##check if the matrix has been calculated already, if so, get from cache
        if (!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        
        ##If the martrix has not been calculated, calculate the matrix below
        dat <- x$get()
        inver <- solve(dat, ...)
        x$setInverse(inver)
        inver
}
