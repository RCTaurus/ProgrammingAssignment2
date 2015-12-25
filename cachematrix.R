## This program creates a cached matrix object 
## which can store a matrix and also an inverse

## This function initializes the object and 
## resets the inverse when ever the value is set.
## This function also defines methods to retrieve
## the value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(z) inv <<- z
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function looks at the object created by makeCacheMatrix
## then looks at the getinv() to see if the inverse was previously 
## calculated and stored, if it was stored earlier it fetches the 
## stored inverse else calculates the inveerse and stores it using
## setinv() for future retrieval.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinv(inv)
  inv
}