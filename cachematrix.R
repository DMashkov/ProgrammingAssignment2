# makeCacheMatrix creates a special "matrix", which containing a function to
#   set the value of the matrix
#   get the value of the matrix
#   set the value of the inverse matrix
#   get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##  Function calculates the inverse matrix 
##  created with the above function
##  If inversed matrix already exists
##  function return it (cache function)

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}