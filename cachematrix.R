## Function to cache original matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv_m = NULL
  set = function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv_m <<- inverse 
  getinv = function() inv_m
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## Function to create inverse matrix ()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_m = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv_m)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv_m)
  }
  
  # otherwise, calculates the inverse 
  mat_inv = x$get()
  inv_m = solve(mat_inv, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv_m)
  
  return(inv_m)
}
