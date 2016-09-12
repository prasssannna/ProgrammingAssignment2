## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Gets or sets a matrix and functions in a new environment.
## Returns a null or a cached inverted matrix
makeCacheMatrix <- function(x = matrix()) {

makeCacheMatrix <- function(x=matrix())
{
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
	        
    get <- function() x
		    
    setInv <- function(mx) im <<- mx
		        
    getInv <- function() im
			    
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
					    }

}


## Write a short comment describing this function
## Returns the inverted matrix if it is available
## If not available from the environment, creates an
## inverted matrix and caches it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im <- x$getInv()

    if (!is.null(im)) {
        message ("Getting cached data")
        return( im)
    }

    data <- x$get()
    im <- solve(data,...)
    x$setInv(im)
    im
}
