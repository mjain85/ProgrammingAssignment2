## Speeding the inversion of a matrix using cache

## This function (makeCacheMatrix) will create an object which holds matrix and its inverse, here is example
## a<-matrix(c(4,2,1,6), nrow = 2) #create matrix
## b<-makeCacheMatrix(a) #create an object with matrix
## b$get() will return original matrix
## cacheSolve(b) will store and return inverse of matrix (a) stored in b
## b$getinvM() will return inverse of matrix a

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## any new matrix set will set m to null so calling cahceSolve will compute inverse and not old inverse
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## to get matrix
  get <- function() x
  ## to set inerse matrix
  setinvM <- function(invM) m <<- invM
  ## to get invese of matrix
  getinvM <- function() m
  list(set = set, get = get,
       setinvM = setinvM,
       getinvM = getinvM)
}


## This function will compute the matrix, if inverse is already computed previously it will return otherwise it will compute the inverse and store

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinvM()
  ## if m is not null, we have computed inverse previosly, return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## get matrix
  data <- x$get()
  ## compute the inverse and store it
  m <- solve(data, ...)
  x$setinvM(m)
  m
}
