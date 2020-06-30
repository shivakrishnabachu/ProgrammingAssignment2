## Put comments here that give an overall description of what your
## functions do
##  Two functions are created to make store a cache of inverse of matrix
## Write a short comment describing this function

##  This function creates special matrix to create a inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y){ 
    x <<- y 
    inv <<- NULL
  } 
  get <- function() x 
  setInverse <- function(solveMatrix) inv <<- solveMatrix 
  getInverse <- function() inv 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}


## Write a short comment describing this function

##  Below function creates a creates or caches a inverse matrix from matrix in above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'   
  inv <- x$getInverse() 
  if(!is.null(inv)){ 
    message("getting cached data")
    return(inv) 
  } 
  data <- x$get() 
  inv <- solve(data) 
  x$setInverse(inv) 
  inv
 }
