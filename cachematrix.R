## The two function implement a repositiory pattern in R.  The
##makeCacheMatrix function gets and sets the data, while the
##cacheSolve implements the business logic that acts on the data

## makeCacheMatrix creates a special object that stores a numeric matrix and
##cache's its inverse.  makeCacheMatrix retrieves the data and maps it to the
##entity model, which is the returned specialObject.  The
##specialObject is a low level class that has an interface
##defined by makeCacheMatrix.  It is the repository.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##cacheSolve  implements the business logic for the solution to the problem.
##The model (data) is passed to it encapsulated in the makeCacheMatrix repository.
##It operates on the model and returns an inverted matrix.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  inputdata <- x$get()
  inverse <- solve(inputdata, ...)
  x$setinverse(inverse)
  inverse
}
