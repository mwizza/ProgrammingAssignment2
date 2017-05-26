## The two functions 'makeCacheMatrix' and 'cacheSolve' take in a matrix ...
## and compute the inverse of the matrix (using solve()) if, and only if, the ...
## inverse does not already exist in stored memory

## makeCacheMatrix function creates a special matrix ...
## whose inverse can be cached (stored in memory)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(aMatrix) {
    x <<- aMatrix
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
}

## cacheSolve computes the inverse of the special matrix returned from makeCacheMatrix
## however the function checks to see if the inverse exists in memory 
## if so, it returns this inverse and avoids redoing the calculation 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

