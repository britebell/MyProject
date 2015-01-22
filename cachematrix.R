## Functions which enable us to get the inverse of a given marix  
## in a way avoiding repeated matrix-inversion operation on the same one

## Function to set up the work environment

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function (y) {
		x <<- y
		m <<- NULL
	}
	get <- function () x
	setInv <- function (mInv) m <<- mInv
	getInv <- function () m
	list (set = set, get = get, setInv = setInv, getInv = getInv)
}


## Cache version of "solve()"

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getInv()
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInv(m)
	m
}
