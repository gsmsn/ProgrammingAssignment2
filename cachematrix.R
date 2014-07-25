## the makeCacheMatrix function creates a special "matrix", which 
## is really a list, and contains the following four functions: 
##   1. set -  sets the value of the matrix
##   2. get -  gets the value of the matrix 
##   3. setInverse - sets the value of the matrix inverse
##   4. getInverse - gets the stored value of the matrix inverse

makeCacheMatrix <- function(x = matrix(numeric(), 0, 0)) {
	im <- NULL
	set <- function(y) {
		x <<- y
		im <<- NULL
	}
	get <- function() x
	setInverse <- function(inverseM) im <<- inverseM
	getInverse <- function() im
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## cacheSolve returns the 'inverse' of a matrix. cacheSolve first 
## checks to see if the inverse has already been calculated (by using
## the makeCacheMatrix function to see if the inverse was previously 
## calculated and stored as a special "matrix").  If so, 
## it gets the inverse from the cache and skips the computation. If 
## not, it calculates the inverse and set the value in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	im <- x$getInverse()
	if(!is.null(im)) {
		message("getting the cached data")
		return(im)
	}
	data <- x$get()
	im <- solve(data, ...)
	x$setInverse(im)
	im
}
