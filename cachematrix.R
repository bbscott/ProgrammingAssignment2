## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## x is matrix passed
  ## dm is inverted matrix
  
  dm <- NULL
  
  set <- function(y){
    x <<- y
    dm <<- NULL
  }
  get <- function() x
  
  ## Create matrix object for caching inverse
  setSolved <- function(solve){
    dm <<- solve
  }
  getSolved <- function() dm
  list(set = set, get = get, setSolved = setSolved, getSolved = getSolved)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Retrieve the cached solution from x
  dm <- x$getSolved()
  ## If it's there, return it
  if(!is.null(dm)){
    print("Retrieving cached solution")
    return(dm)
  }
  
  ## Otherwise, get the "matrix" and solve for the inverted matrix
  data <- x$get()
  dm <- solve(data, ...)
  ## Then cache the solved matrix
  x$setSolved(dm)
  dm
  
}
