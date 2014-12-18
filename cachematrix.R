## These functions allow the result of the potentially time-consuming 
## matrix inverse to be cached with the underlying matrix.
## makeCacheMatrix creates a special "matrix" object with its cached inverse
## which is calculated by the cacheSolve function

## makeCachMatrix: Stores a matrix and caches its inverse
## Parameter: x is a matrix to be stored (note that x is assumed to be invertible).
## Note: The inverse is calculated with the cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
		
		## Store the matrix and reset the cached inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
		
		## Return the matrix
        get <- function() x
		
		## Store the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
		
		## Return the inverse of the matrix
        getinverse <- function() inv
		
		## Add the functions defined above to a list
		## so they can be accessed by e.g. make
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: Returns the previously calculated inverse stored by the makeCacheMatrix function
## or if the underlying matrix has changed, re-calculates the inverse and caches it.
## Parameter: x the "matrix" object returned by the makeCacheMatrix function
cacheSolve <- function(x, ...) {
		
		## Get the inverse
		inv <- x$getinverse()
		
		## Return the inverse if it has been previously calculated
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
		
		## Get the underlying matrix
        data <- x$get()
		
		## Calculate its inverse
        inv <- solve(data, ...)
		
		## Store the inverse and return it
        x$setinverse(inv)
        inv
}
