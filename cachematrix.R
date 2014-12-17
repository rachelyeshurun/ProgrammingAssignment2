## These functions create a matrix object and associated getter and setter functions.
## The object also has an setinverse and getinverse functions to set and get the matrix inverse.
## The matrix inverse is cached, so it is calculated only the first time getinverse is called.
## The assumption is that the input matrix in invertible

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  
  # set function clears the cached inverse
  set <- function(y) {
      x <<- y
      matrixInverse <<- NULL
  }
  # get function returns the object
  get <- function() x
  
  # setinverse stores the matrix inverse
  setinverse <- function(inverse) matrixInverse <<- inverse
  
  # getinverse returns the cached matrix inverse
  getinverse <- function() matrixInverse
  
  # function returns this object's functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  matrixInverse <- x$getinverse()
  if(!is.null(matrixInverse)) {
      message("getting cached data")
      return(matrixInverse)
  }
  
  # cached data was null, need to calculate the inverse
  data <- x$get()
  matrixInverse <- solve(data, ...)
  
  # cache the calculated inverse
  x$setinverse(matrixInverse)
  matrixInverse
}
