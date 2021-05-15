## The two functions help in providing the inverse of a matrix. If the same 
## matrix is supplied again then the cached inverse is returned.

## It makes a special matrix which can then be passed to cacheSolve()

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL
  }
  get <- function() m
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function first checks if a cached value is available for the provided
## matrix. If not, it calculates and returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}
