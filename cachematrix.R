## The two functions are to create a special object that stores a matrix and caches its inverse.

## Function makeCacheMatrix creates a matrix, storing a list of 4 functions: set, get, setsolve, getsolve.
## Function get returns the vector x stored in Function makeCacheMatrix; Function set changes the vector stored in makeCacheMatrix.
## setsolve & getsolve don't calculate the inverse of the matrix, but store an input value into the main function and return it. 

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        	set <- function(y) {
                x <<- y
                m <<- NULL
        	}
        	get <- function() x
        	setsolve <- function(solve) m <<- solve
        	getsolve <- function() m
        	list(set = set, get = get,
             	setsolve = setsolve,
             	getsolve = getsolve)

}


## Function cacheSolve uses the matrix created before, calculates and caches its inverse matrix 
## If the inverse matrix has already been calculated, directly get it from the cache and skip the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getsolve()
        	if(!is.null(m)) {
                message("getting cached data")
                return(m)
        	}
	## If x$getsolve() is called a second time, the cache statement will be displayed.
 
        	data <- x$get()
        	m <- solve(data, ...)
        	x$setsolve(m)
        	m
}
