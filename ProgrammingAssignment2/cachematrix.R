## Following function is written calculate and cache the inverse matrix.
## It would save the time on re-calculation as the value is cached.

## This function creates a matrix, which includes seeting and getting the 
## value of the matrix, setting and getting the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function first checks whether the inverse of the matrix is calculated.
## If already calculated and the matrix has not changed,
## then it takes the cached value of the inverse of the matrix
## If it's not calculated, then it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
