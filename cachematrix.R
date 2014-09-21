## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly.The below functions compute and cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<-Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The following function calculates the inverse matrix of the special "vector" created with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise,
## it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
		if(!is.null(m)) {
			message("getting cached data")
			return(m)
	}
	data <- x$get()
	m <- Solve(data, ...)
	x$setInverse(m)
	m
}
