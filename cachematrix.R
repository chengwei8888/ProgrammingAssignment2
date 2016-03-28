## Put comments here that give an overall description of what your
## functions do

##Write a short comment describing this function

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the solve
##get the value of the solve


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  

}


## Write a short comment describing this function
##The function calculate the solve of a special "matrix" created with the above function.
##It first check to see if the solve has been calculated
##If so, it gets the solve from  the cache and skips the computation
##Otherwise, it calculate the solve of the data and sets the value of the solve in the cache via the setsolve function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
        
}
