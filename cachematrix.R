## A set of functions that create an object that can store a matrix, compute its
## inverse and store its inverse in a cache

## Creates a special "matrix" object that can cache its inverse in a list

makeCacheMatrix <- function(m = matrix()) {
  i <- matrix()
  set <- function(x) {
    m <<- x
    i <<- matrix()
  }
  get    <- function() m
  setInv <- function(inv) i <<- inv
  getInv <- function() i
  list(set    = set, 
       get    = get,
       setInv = setInv,
       getInv = getInv)
}

## Computes and prints the inverse of the matrix set in makeCacheMatrix
## if the inverse has already been calculated,it is instead retrieved from cache

cacheSolve <- function(m, ...) {
  i <- m$getInv()
  if (!is.na(i)[1]) {
    message("Getting cached data...")
    return(i)
  }
  data <- m$get()
  i    <- solve(data, ...)
  m$setInv(i)
  i
}  

