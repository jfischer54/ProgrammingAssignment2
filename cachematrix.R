## These 2 functions work together to cache in memory the matrix and matrix inverse. This allows the cached inverted matrix 
## to be output rather than the inverse re-calculated for subsequent requests of the inverse of the same matrix.

## The first function, makeCachematrix, creates and outputs a list with the 4 functions that set the value and allow retrieval of the matrix and
## its inverse assigned to 4 objects

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverseMatrix <<- inverse
  getinverse <- function() inverseMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve either calculates and saves the inverse of the marix using the solve function or returns the previously 
## calculated (cached) inverse depending on whether this is the initial or subsequent request 
## for the invert of that matrix
  
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getinverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setinverse(inverseMatrix)
  inverseMatrix
}
