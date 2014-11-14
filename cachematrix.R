## These functions cache the inverse of a matrix.

## This function creates a special matrix, with setters and getters 
## for its own value, and its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setsolve <- function(s) inv <<- s
	getsolve <- function() inv
	list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## This function returns the inverse of a matrix. If the inverse of the
## given matrix is already cached, return the cached inverse without
## repetitive computation. Otherwise, compute the inverse and cache it.

cacheSolve <- function(x, ...) {
	inv <- x$getsolve()
	if (!is.null(inv)){
		message("getting cached inverse")
		return(inv)
	}
	inv <- solve(x$get())
	x$setsolve(inv)
	inv
}	