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
## .... outputs inverse from cache ...

## Creates a special "Matrix" which is really a list
## contains a function to:
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
## of the matrix
##
## USAGE: cacheSolve(x, ...) - where 'x' is a
## 'makeCacheMatrix' object
##
## (if it doesn't exist in cache it calculates it
## and caches before returning)
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