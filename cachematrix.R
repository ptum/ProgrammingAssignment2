## Caching the Inverse of a Matrix
## uses solve() to calculate the inverse
## of a given matrix and by taking advantage of
## R's scoping rules is able to cache it's results.
##
## Complete Example:
## hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## h8 <- hilbert(8); h8 
## s <- makeCacheMatrix(h8)
## sh8 <- cacheSolve(s)
## round(sh8 %*% h8, 3) # check if correctness of the results [AA' = I]

## Creates a  "Matrix" object with 4 methods to: 
## set value, get value, set the inverse of matrix and get the inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(solve) m <<- solve
	getinv <- function() m
	list(set = set, get = get,
			setinv = setinv,
			getinv = getinv)
	
}


## Given a makeCacheMatrix object, returns the inverse
## of the matrix if it precalculated; if not find the inverse and set the value of inverse
##
## USAGE: cacheSolve(x, ...) - where 'x' is a 'makeCacheMatrix' object
##

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getinv()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
}