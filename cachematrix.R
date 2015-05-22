# takes a matrix x and returns a caching object based on x that can store the inverse of x
# (matrix) set - resets the stored matrix x, clears the cache, and returns x
# (matrix) get - returns the original matrix x
# (matrix) setinverse - stores and returns the cached inverse of the matrix x
# (matrix) getinverse - returns the cached inverse of matrix x (if it is stored)
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
        i <<- NULL
        x <<- y
        x
  }
  
  get <- function() x
  
  setinverse <- function(inverse){
        i <<- inverse
        i
  }   
  
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# takes a caching matrix x, solves for its inverse, caches the result, and 
# returns the result. subsequent calls will return the cached result
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
  if(!is.null(i)) {
        message("returning cached data")
        return(i)
  }
  
  i <- solve(x$get(), ...)
  
  x$setinverse(i)
}
