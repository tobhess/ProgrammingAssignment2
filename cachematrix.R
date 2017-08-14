## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The first function, makeVector creates a special "vector", which is really a list containing a function to
#   1. set the value of the matrix
#   2. get the value of the matrix
#   3. set the value of the inverse
#   4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  iv <- NULL
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  get   <- function() x
  setiv <- function(solve) iv <<- solve
  getiv <- function() iv
  list(set = set, get = get,
       setiv = setiv,
       getiv = getiv)
}


## Write a short comment describing this function
# The following function calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setiv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  iv <- x$getiv()
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  iv <- solve(data, ...)
  x$setiv(iv)
  iv
}
