## the following functions allow calculation of the inverse of an invertable
## matrix. If previously cacheSolve is called for the a previously solved object, 
## it will return the cached value rather than recalculating


## makeCacheMatrix creates a list matrix which will allow caching of 
## inverse solutions
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
	
	
## cacheSolve checks to see if makeCacheMatrix has been called for a particular matrix
## previously and if so retrieves the previously calculated inverse solution

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}


