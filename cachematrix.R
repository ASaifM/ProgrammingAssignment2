## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix maintains the matrix and
## its inverse through getters and setters.
## cacheSolve is used to calculate the inverse
## once and then cache the result for future
## retrievals.

## Write a short comment describing this function
## makeCacheMatrix initially takes a matrix
## called x. One can then retrieve or manipulate
## the value of x through get and set respectively.
## i is used to store the value of the inverse
## of x. getinverse is used to get the value
## of i and setinverse is used to set the value
## of i.
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve gets a matrix as a parameter. It then
## checks if the matrix has its inverse computed or
## not. If it is the case, the cached value is
## returned, otherwise it is computed using the
## function solve and stored for the matrix x to
## avoid computing it again in the future.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting the cached inverse of matrix")
		return(i)
	}
	data <- x$get()
	i <- solve(data)
	x$setinverse(i)
	i
}
