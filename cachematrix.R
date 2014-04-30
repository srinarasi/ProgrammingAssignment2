## This is a class to cache the inverse of a matrix and return it when necessary
## Calculating the inverse of a matrix is a costly operation and
## it makes sense to cache the value that we calculate and return it when it's needed

## This function returns a list of functions which have access to the matrix and the cached value of th einverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
	## Set the new matrix and reset the inverse to NULL
		x <<- y
		inv <<- NULL
	}
	get <- function() x

	## Getter and setter for the inverse
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function calculates the inverse and caches it for future use

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if (!is.null(inv)) {
	## if getInverse returned a non-NULL valuse, it means that we already
	## have a cached value. Return it.
		message("getting cached data")
		return(inv)
	}

	## If we don't have a cached value, calculate the inverse, cache it in a
	## variable and return the new value.

	data <- x$get()
	inv <- solve(data, ...)
	x$setInverse(inv)
	inv

}
