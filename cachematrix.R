## Exploring the benefit to caching the inverse of a matrix rather than compute it repeatedly 
## The pair of functions below cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y) {
			x <<- y
			m <<- NULL }
		get <- function() x
		setinverse <- function(inverse) m <<- inverse
		getinverse <- function() m
		list( set=set, 
			get=get, 
			setinverse=setinverse, 
			getinverse=getinverse)
}


## cacheSolve: This function takes a matrix and returns its inverse
## It first checks to see if the inverse has already been computed.
## If so, it gets the inverse from the cache and skips the computation

cacheSolve <- function(x, ...) {
		## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
		if(!is.null(m)) {
			message("Getting cached data...")
			return(m)
		}
		data <- x$get()
		m <- solve(data)
		x$setinverse(m)
		m
}
