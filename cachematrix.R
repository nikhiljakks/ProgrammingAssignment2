## Put comments here that give an overall description of what your
## The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the mean
# get the value of the mean
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	temp <- list ()
	length (temp) <- length (x)
	dim(temp) <- dim (x)
	set <- function(y) {
			x <<- y
			temp <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) temp <<- solve
	getinverse <- function() temp
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## Write a short comment describing this function
# The following function calculates the Inverse of the special "Matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        temp <- x$getinverse()
        if(!is.null(temp[[1,1]])) {
			message("getting cached data")
			return(temp)
        }
        data <- x$get()
        temp <- solve(data)
        x$setinverse(temp)
        temp
}
