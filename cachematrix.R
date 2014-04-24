## "Matrix inversion is usually a costly computation", to these functions set up caching functionality
## for inverting matrices.

## makeCacheMatrix()
## 
## Sets up caching functionality for matrices.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(inv) i <<- inv
  getInv <- function() i
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve()
## 
## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    i <- x$getInv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setInv(i)
    i
}
