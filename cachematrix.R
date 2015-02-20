# These two functions, for a given matrix, caches and returns its inverse.
# If the inverse matrix has previously been calculated, it is retrieved from
# the cache (not calculated again).

# Defines the functions necessary for cacheSolve to work
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   
   # Defines a function that, when called, will set x to the parameter it
   # is called on, and clear any prior value of m.
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   
   # When called, will return the value of x (i.e. the original matrix)
   get <- function() x
   
   
   # When called, will set 'm' to the inverse of 'x'
   setInverse <- function(solve) m <<- solve
   
   # Will return the value of 'm'
   getInverse <- function() m
   
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}
   


# Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

   # Set m to the cached inverse of 'x' (or NULL if it hasn't been
   # calculated)
   m <- x$getInverse()
   
   # If the cache isn't empty, return its contents (the inverse of 'x')
   if(!is.null(m)) {
      message("getting cached data")
      return(m)     # (If the inverse had been cached, the function
      # prints the inverse and exits here)
   }
   
   # To reach here, the cache must have been empty. Calculate and cache
   # the inverse now.
   data <- x$get()
   m <- solve(data, ...)
   x$setInverse(m)
   
   # Print the inverse matrix
   m
}
