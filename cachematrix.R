# The functions below provide support for a matrix class that can cache its own inverse
# 
# makeCacheMatrix(x=matrix()) : create a new instance with underlying matrix x
# cacheSolve(x, ...) : calculate, cache, and return the inverse of x, using options ...
#
# To manipulate cache matrix x:  
# x$set(m) : set the value of matrix x to matrix m, and clear inverse cache
# x$get() : returns the matrix x
# x$setinv(m) : manually set the inverse of x to matrix m
# x$getinv() : returns the cached inverse of x

makeCacheMatrix <- function(x = matrix()) {
  # Create a new cache matrix
  # Arguments: 
  # x : the underlying matrix
  inv <- null
  set <- function(y) {
    # Replace the underlying matrix and clear the cached inverse
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <- inverse
  getinv <- function() inv
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, tolerance = 1.5e-8, ...) {
  # Calculate, cache, and return the inverse of cache matrix x
  # Arguments:
  # tolerance : sets the tolerance on the inverse check
  # ... : provides further arguments passed to the inverting function (solve())
  # get the data from x
  m <- x$get()
  inv <- x$getinv()
  # construct the identity matrix
  id <- diag(ncol(x))
  if(!is.null(inv) & isTRUE(all.equal(id, m %*% inv, tolerance))) {
    # if the value in cache is not null and 
    # we actually have the inverse (to within tolerance), 
    # then return the value in cache
    message('getting cached data')
    return(inv)
  }
  # for further development: check here that x is invertible
  # solve() with options to calculate the inverse
  inv <- solve(m, ...)
  # store the inverse in the cache
  x$setinv(inv)
  # and return it
  inv
}
