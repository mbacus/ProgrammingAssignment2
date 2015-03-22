## The following code covers a pair of functions:

## makeCacheMatrix() can cache the inverse of a square matrix, while
## cacheSolve() returns the inverse of the matrix makeCacheMatrix() returns.

## The makeCacheMatrix() function creates a special "matrix" object 
## that can cache its inverse. Given a matrix x, this function returns 
## a list containing the following functions:
## 1. set                   sets the value of the matrix
## 2. get                   gets the value of the matrix
## 3. setInverse            sets the value of the inverse of the matrix
## 4. getInverse            gets the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
      
	        m <- NULL
	        ## initially sets the cache to NULL
                
	        set <- function(y) {
	        ## this stores a new matrix

        		        x <<- y	
        		        m <<- NULL
        		        ## sets the cache to NULL since we have a new matrix
	        }

	        get <- function() x  	
	        ## returns the stored matrix

	        setInverse <- function(solve) m <<- solve
	        ## caches the inverse
	        
	        getInverse <- function () m
	        ## gets the cached inverse

	        list(set = set, get = get, 
		        setInverse = setInverse, 
		        getInverse = getInverse)
	        ## returns a list of the functions above

}


## The cacheSolve() function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix() above. If the inverse has already been 
## solved (and has not changed), cacheSolve() retrieves the inverse 
## from the cache.

cacheSolve <- function(x, ...) {

	        m <- x$getInverse()
	        ## gets the cached inverse
        
	        if(!is.null(m)) {
	        ## if the cache is not NULL
	        ## this retrieves the inverse from the cache

        		        message("getting cached data")
        		        return(m)
	        }

	        mNew <- x$get()
	        ## gets the new matrix
	        
	        m <- solve(mNew)
	        ## solves the inverse of the new matrix
	        
	        x$setInverse(m)
	        ## caches the inverse
	        
	        m
	        ## returns the inverse of x

}
