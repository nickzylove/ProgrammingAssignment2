## a pair of functions that cache the inverse of a matrix.

## The function makeCacheMatrix() creates a special "matrix"
## object that can cache its inverse;

makeCacheMatrix <- function(x = matrix()) {

	inverseX <- NULL
	set <- function(y){
		x <<- y
		inverseX <<- NULL
	}
	get <- function() x
	setSolve <- function(solve) inverseX <<- solve 
	getSolve <- function() inverseX

	list(set=set, get=get,
		setSolve=setSolve,
		getSolve=getSolve)
	
}


## Then the function cacheSolve() computes the inverse of 
## the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverseX <- x$getSolve()

	if(!is.null(inverseX)){
		message("getting cached data")
		return(inverseX)
	}

	data <- x$get()
	inverseX <- solve(data)
	x$setSolve(inverseX)
	inverseX
}
