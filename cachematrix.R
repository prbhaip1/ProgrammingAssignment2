## This program has two functions makeCacheMatrix & cacheSolve.
## It calculates the inverse of an input matrix.If the inverse is already
## calculated for a given input in the memory then the value is returned from the
## memory else the value is calculated.


##function makeCacheMatrix has four functions:
## set() : sets/changes the value of the vector stored in main func
## get() : gets the returns of the vector from the main function
## setInverse() : sets the value of inverse in cache
## getInverse() : gets the value of inverse from the cache


makeCacheMatrix <- function(x = matrix()) { 
  # Input to this function is a matrix 
  # Return  special "matrix" object
  i <- NULL # assign NULL to m
  set <- function(y) {
    x <<- y # substitues the vector x with y
    i <<- NULL # restores the value of m to NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  #the following line stores the 4 functions in a list:
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve checks if inverse i is stored previously with getInverse. If it is 
## stored in the memory it returns i, else if computes the inverse and return i


cacheSolve <- function(x, ...) {
  # Input special "matrix" object
  # Return a matrix that is the inverse of 'x'
  i <- x$getInverse()  # get the cached result
  # check if data is in cache (not null) and not waste compute time 
  if(!is.null(i)) {
    message("getting cached data") 
    return(i) # Return a cached data that is the inverse of 'x' 
  }
  # else compute the inverse
  data <- x$get()
  i <- solve(data, ...) 
  x$setInverse(i)
  i
}
