## These functions cache and recall the inverse of a given square matrix x.

## The makeCacheMatrix function produces a list of functions which return the
## matrix itself and an inverse matrix matched to variables internal to the
## function, as well as calls to return them to the workspace.

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


## The cacheSolve function computes and stores in the workspace variable m the
## inverse of matrix x above, and returns it.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached matrix")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
