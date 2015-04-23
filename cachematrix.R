## This code calculates the inverse of a Matrix with cached results for better performance.
## In the end of code we use 'result_01' to receive the result of makeCacheMatrix using some parameters
## Run 'result_inv <- cacheSolve(result_01)' 2 times to see the cached result

## Create a Matrix and use Get and Set methods
makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL 
		set <- function(y) { 
		x <<- y 
		m <<- NULL 
        } 
			get <- function() x 
			setmatrix <- function(solve) m <<- solve 
			getmatrix <- function() m 
			list(set = set, get = get, 
			setmatrix = setmatrix, 
			getmatrix = getmatrix) 
} 


## cacheSolve find the inverse of a matrix and check if it is cached
cacheSolve <- function(x = matrix(), ...) {

        ## Return a matrix that is the inverse of 'x'        
		m <- x$getmatrix() 
		if(!is.null(m)) { 
		message("Get cached data") 
		return(m) 
        } 
			data <- x$get() 
			m <- solve(data, ...) 
			x$setmatrix(m) 
			m 
} 

result_01 <- makeCacheMatrix(matrix(1:10, 2, 2))
result_inv <- cacheSolve(result_01)  ## Run this line again to see the cached result