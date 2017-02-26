## function makeCacheMatrix returns :
## a list containing 4 functions : 
## -> set, which allows to set another value of matrix that we want to inverse ; 
## -> get, which allows to read the value of the matrix that we want to inverse ; 
## -> setinverse, which allows to set the value of the inverse of the matrix that we have previously set ; (warning : no operation is performed here. It just sets its value.)
## -> getinverse, which allows to read the inverse of the matrix. (Will be "NULL" if has never been set)

makeCacheMatrix <- function(x = matrix()) {
	invrs <- NULL 
	set <- function(y){ 
		x <<- y 
		invrs <<- NULL 
	}
	get <- function() x 
	setinverse <- function(solve) invrs <<- solve
	getinverse <- function() invrs
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## cachesolve returns the inverse of a matrix.
## IF the inverse of this matrix has never been calculated, it performs required calculation, cache the result (the inverse of the matrix) and then returns this result.
## IF the inverse of this matrix has already been calculated and cached, it returns this cached value and prints a message saying that this has already been calculated, and returning a cached value. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		invrs <- x$getinverse()
		
		if(!is.null(invrs)){
		message ("Return cached data")
		return(invrs)
		}
		
		data <- x$get()
		invrs <- solve(data)
		x$setinverse(invrs)
		invrs
}
