# Matrix inversion is usually a costly computation task.
# Therefore, rather than repeating the computation, caching the inverse of a matrix may be beneficial.
# Functions makeCacheMatrix and cacheSolve below demonstrate this task.

# The function makeCacheMatrix creates a special "matrix", which is a list containing a function to
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

      m <- NULL
      set <- function(y) {
          x <<- y
          m <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) m <<- solve
      getInverse <- function() m
      list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  
}


# The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix function above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        m <- x$getInverse()
        if(!is.null(m)) {
              message("getting cached data")
              return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m		
}
