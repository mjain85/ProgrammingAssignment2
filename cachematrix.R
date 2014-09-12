## Speeding the inversion of a matrix using cache

## This function will create an object which holds matrix and its inverse, here is example
## a<-matrix(c(4,2,1,6), nrow = 2) 
## b<-makeCacheMatrix(a)
## b$get() will return matrix
##cacheSolve(b) will store and return inverse of b

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvM <- function(invM) m <<- invM
  getinvM <- function() m
  list(set = set, get = get,
       setinvM = setinvM,
       getinvM = getinvM)
}


## This function will compute the matrix, if inverse is already computed previously it will return otherwise it will compute the inverse and store

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinvM()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvM(m)
  m
}
