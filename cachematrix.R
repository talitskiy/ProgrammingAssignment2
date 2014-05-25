## "makeCacheMatrix" is a function that creates 
## an empty matrix in cache suitable for the storage 
## of an iverted matrix, which is to be computed afterwards 
## by another function, "cacheSolve". 

## You can see below how the "makeCacheMatrix" function creates an empty matrix 
## and a list of functions to be used further when the "cacheSolve" function 
## computes the inverse of the special "matrix" 
## returned by the "makeCacheMatrix" function.
 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmtx <- function(mtx) m <<- mtx
  getmtx <- function() m
  super<<- list(set = set, get = get,
       setmtx = setmtx,
       getmtx = getmtx)
}


## This function computes the inverse of the special "matrix" 
## returned by "makeCacheMatrix" function above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## the "cacheSolve" function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- super$getmtx()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- super$get()
  m <- solve(data, ...)
  super$setmtx(m)
  m
}
