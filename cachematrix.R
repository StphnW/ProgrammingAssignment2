# The makeCacheMatrix function allows you to create a matrix that you can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve<- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#The cacheSolve function allows you to generage the inverse of the matrix created with the function makeCacheMatrix above either by retrieving it from the cache or by creating one using the solve() function if it does not already exist.

cacheSolve <- function(x, ...) {
  m <- x$getsolve() 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  ## If inverse has been created already, the message "getting cached data" will appear and then the inverse from the cache
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  ##takes the inverse
  m
  ##prints the inverse of the original matrix
}
