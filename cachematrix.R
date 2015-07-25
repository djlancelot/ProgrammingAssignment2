#Usage:
# 1. create a matrix, like A = matrix(c(1,0,0,1), 2,2).
# 2. create and store the cacheable object using myA <- makeCacheMatrix(A)
# 3. calculate the inverse using cacheSolve(myA)

## This function creates the object with the cached inv matrix in it,
# which defaults to NULL. The helper function changes its value.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(mean) inv <<- mean
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function checks if we have a cached inverse matrix and 
# return that if we do. Otherwise we calculate the inverse and
# return the answer in the end.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    # You can check if it returns a cached data by uncommenting 
    # the line below:
    # message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
