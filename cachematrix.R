## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The purpose of this function is to create a matrix that will have the ability to cache the inverse, so that it does not need to be recomputed every time it needs to be called.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
			set <- function(z){
					x <<- z
					inv <<- NULL
				
				
			}
			get <- function() x
			giveinverse <- function(inverse) inv <<- inverse
			retrieveinverse <- function() inv
			list(set = set,
				 get = get,
				 giveinverse = giveinverse
				 retrieveinverse = retrieveinverse)

}


## Write a short comment describing this function
## The purpose of the function is to return the inverse of the matrix from the above function. Using an logic expression, if the the inverse has previously been returned, then this function will use the cached data to retrieve the inverse. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$retrieveinverse()
        if(!is.null(inv)) {
        		message("retrieving cached data")
        		return(inv)  	
        }
        mat <- x$get()
        inv <- solve(mat,...)
        x$setInverse(inv)
        inv  
}
