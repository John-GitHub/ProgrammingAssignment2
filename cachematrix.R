## The makeCacheMatrix function takes a matrix and wraps it in a list object that contains
## the original matrix, an environment for presistent storage of an inverse, and accessor funtions.
##
## The cashSolve function extracts the matrix inverse from an object returned by the makeCacheMatrix function
## if it exists, and sets the persistent inverse in the object to the calculated inverse otherwise.


## This function creates a wraper for a matrix that contains an environment for the presistent storage of its inverse.

makeCacheMatrix <- function(x = matrix()) {
  lInv <- NULL
  set <- function(y) {
    x <<- y
    lInv <<- NULL
  }
  get <- function() {x}
  setInv <- function(inv) {lInv <<- inv}
  getInv <- function() {lInv}
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function accesses the stored inverse if it exists and sets it to the calculated inverse if it does not exist.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          lInv <- x$getInv()
  if(!is.null(lInv)) {
    message("getting cached data")
    return(lInv)
  }
  data <- x$get()
  lInv <- solve(data, ...)
  x$setInv(lInv)
  lInv
}
