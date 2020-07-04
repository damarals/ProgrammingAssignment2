## This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(X = matrix()) {
  inv <- NULL
  set <- function(Y) {
    X <<- Y
    inv <<- NULL
  }
  get <- function() X
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(X, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- X$getinv()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  mtx <- X$get()
  inv <- solve(mtx, ...)
  X$setinv(inv)
  inv
}
