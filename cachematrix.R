
## The Cached Matrix is defined by this function for the further use.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Here now the function will be set to values of x,m in parent env.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Here get function is used to retrive x from parent env.
  get <- function() x
  ## Here setinv is used to set the value of m from parent env.
  setinv <- function(solve) m <<- solve
  ## Here getinv function is used to retrive m from parent env.
  getinv <- function() m
  ## Here list is used to return the named functions.
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function returns the inverse matrix that is cached 
## by previous function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  ## Here if loop is used to retrive the value of m from cache if not null.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Here data is assigned get function as defined in previous function. 
  data <- x$get()
  ## Here m is assigned inverse of 'x'matrix
  m <- solve(data, ...)
  ## Here setinv function is retrived, as defined in previous function.
  x$setinv(m)
  ## m is returned
  m
}
