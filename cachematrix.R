## Below are two functions that are used to create a special object that stores a matrix and
## caches the inverse of a matrix.

## The first function, makeCacheMatrix creates a special Matrix, which is really a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the matrix created with the above function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
	if(!is.null(m)){	## if the inverse is already in the cache.
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}