## Pair of functions that cache the inverse of a matrix.
## A test case also shown where inverse is pulled from cache

## This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.  If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
  ## Return a matrix that is the inverse of 'x'
}


# Below is a test case for 2x2 matrix. CacheSolve is called twice.
# In first call of CacheSolve, inverse is not in chache, hence it 
# is calulated and cahed.
# In Second call of CacheSolve, inverse is pulled from the cahe, 
# which is evident from the output message.  

x <- matrix(1:4,2,2) # initializing input matrix
c <- makeCacheMatrix(x) # making special matrix
cacheSolve(c) #inverse not in cahe and calculated
cacheSolve(c) #inverse pulled from cahe

