## These functions take a matrix as input and calculate it's inverse. The inverse is cached so that it can be accessed later

## makeCacheMatrix takes a matrix as input and calculates the inverse. The inverse and matrix itself are outputs to a list. 

makeCacheMatrix <- function(x = matrix()) {
	inverseCache <- NULL
	get <- function() {x}
	setinverse <- function(solve)
			{inverseCache <<- solve}
	getinverse <- function() {inverseCache}
	list(get = get , setinverse = setinverse , getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix. It first checks to see if the inverse has already been calculated. If so, it returns the cached inverse. If not, it calculates the inverse and returns it, storing the inversion in the same object as makeCacheMatrix. 

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	inverseCache <- x$getinverse
	if(!is.null(inverseCache)){
		message('getting cached matrix inversion')
		return(inverseCache)
	}
	matrixdata <- x$get()
	inverseCache <- solve(matrixdata)
	x$setinverse(inverseCache)
	inverseCache
}
