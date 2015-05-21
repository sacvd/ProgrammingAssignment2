
## makeCacheMatrix function receive a square invertible matrix and it creates a list
## of functions to set and get the value of the matrix. In addition other to fucntions
## to set and get the inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse<- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix,setinverse= setinverse,getinverse = getinverse)
}


# cacheSolve function returns the inverse of a matrix, by querying if the matrix's
# inverse has been calculated before. If it was calculated before it gets the stored 
# value and the function print a message and return theinverse matrix stored value.
# If the inverse hasn't been calculated before, the function do the computing and it
# stores the inverse value on the cache by using setinverse function from makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data)
  x$setinverse(m)
   m
}
