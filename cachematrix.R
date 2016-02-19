# The following two functions are used to cache the inverse of a matrix

# makeCacheMatrix creates a list containing a function to 
# set the value of the matrix, get that value, set the 
# value of the inverse of the matrix, and get the value of 
# the inverse.  
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve returns the inverse of the matrix. It checks
# to see if the inverse has already been computed, and if
# so retrives from the cached value. If not, it calculates
# the inverse and sets teh value in the cache. 
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }
        data <-x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
