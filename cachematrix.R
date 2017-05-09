## Put comments here that give an overall description of what your
## functions do

## This function will create an object "matrix" that will be able to cache its inverse rather than computing repeatedly. 
## Matrix inversion takes a lot of resources, so this will make things easie. It is creating a function that sets the value (x) as a new y
## and saves it as cache.

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
## once we have the matrix above, using "cachesolve" we can find the inverse with getInverse.

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
