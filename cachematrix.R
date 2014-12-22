## The following functions are used to create a special matrix that can cache its inverse.
## the inverse is only computed once and cached for further usage, so avoiding computational waste

## This function create a special matrix as a object, whose value and inverse value 
## can be set (determined) and get (retrieved) 

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function ensures that the inverse of the special matrix is computed onlyif it has  
## not been cached yet
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
