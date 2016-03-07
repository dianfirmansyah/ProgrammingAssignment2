makeCacheMatrix <- function(x = matrix()) {
	
	inv <- NULL      	## First, set inv as null	
        set <- function(y) {   	## set function : to input new matrix
                x <<- y
                inv <<- NULL
        }	
        get <- function() x	## get function : to show inputed matrix	
        setInverse <- function(inverse) inv <<- inverse  	## setinverse function : to set new inverse
        getInverse <- function() inv				## getinverse function : to get the newest inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
	inv <- x$getInverse()	## first, set inv with inverse that we get before
        if (!is.null(inv)) {	## Checking is matrix 'x' never calculated before or not
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()		## set mat as matrix 'x'
        inv <- solve(mat, ...)	## calculate inverse of matrix 'x'
        x$setInverse(inv)	## save inv
        inv			## show the inverse of matrix 'x'
}
