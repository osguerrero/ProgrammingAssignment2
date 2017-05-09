## Put comments here that give an overall description of what your
## functions do

## This function will create an object "matrix" that will be able to cache its inverse rather than computing repeatedly. Matrix inversion takes a lot, so this will make things easier.

makeCacheMatrix <- function(x = matrix()) {
					m <-NULL
					set <- function(y){
						x<<-y
						m<<-NULL
					}
					get <- function()x
					setInverse <- function(inverse) m<<-inverse
					getInverse <- function()m
					list(set = set,
						get = get,
						setInverse = setInverse,
						getInverse = getInverse)
}


## This function CacheSolve will calculate the inverse of the "makeCacheMatrix" matrix calculated above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if (!is.null(m)) {
        			message("getting cached data")
        			return(m)
        }
        mat <- x$get()
        m <- solve(mat, ...)
        x$setInverse(m)
        m
}
