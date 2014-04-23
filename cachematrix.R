## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
		##initiate inverse_mtrx to be NULL
		inverse_mtrx <- NULL
		
		##set the matrix information to and new value
		##reset inverse_mtrix to be NULL
		setmatrix <- function (othr_mtrx) {
				x <<- othr_mtrx
				inverse_mtrx <<- NULL
		}
		
		##get the matrix information
		getmatrix <- function() x
		
		##cache inverse
		setinverse <- function(inverse) inverse_mtrx <<- inverse
		
		##get inverse matrix
		getinverse <- function() inverse_mtrx
		
		##return a list of functions
		list(set = setmatrix, get = getmatrix, 
		setinverse = setinverse, 
		getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##get cached inverse matrix
		inverse_mtrx <- x$getinverse()
		
		##if no inverse matrix is cached
		##get the original matrix to compute
		##then cache result 
		if (is.null(inverse_mtrx)) {
				mtrx <- x$get()
				inverse_mtrx <- solve(mtrx)
				x$setinverse(inverse_mtrx)
		}
		
		##return inverse matrix
		inverse_mtrx
}
