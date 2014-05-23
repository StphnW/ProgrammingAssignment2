# The makeCacheMatrix function allows you to create a matrix that you can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {   #function takes in values that is assigned for x

  m <- NULL 
 
  set <- function(y) { # Four nested fuctions are set below (set(), get(),setsolve(),getsolve()), 
    x <<- y
    m <<- NULL
  }#if values are passed thruogh set() function, the vector is stored in x and clears cache m by assigning "NULL" to it.   They symbol <<- alows the values x and m to be altered dynamically in parent function (makeCacheMatrix)
  
  get <- function() x  # get() function returns the values stored in x
  setsolve<- function(solve) m <<- solve #setsolve() stores values passed through it in m (the cache)
  getsolve <- function() m #getsolve retrieves the the values stored in m 
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#The cacheSolve function allows you to generate the inverse of the matrix created with the function makeCacheMatrix above either by retrieving it from the cache or by creating one using the solve() function if it does not already exist.

cacheSolve <- function(x, ...) {
  m <- x$getsolve() 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } # If inverse has been created already, the message "getting cached data" and the inverse values will be returned.
  data <- x$get() 
  m <- solve(data, ...)
  x$setsolve(m) 
  m  ##prints the inverse
}
