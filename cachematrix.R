## The pair of functions makeCacheMatrix and cacheSolve work together.
## The makeCacheMatrix accepts a matrix and create some functions 
## including the calculation of the inverse matrix.
## The cacheSolve function optimizes the getinverse function created in 
## makeCacheMatrix by providing the cached inverse of a matrix if it was 
## previously calculated and the matrix is unchanged


## This function creates a matrix object that can cache its inverse
## It returns a list of these functions: set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() { x }                     # create get function   
  setinverse <- function(solve) {i <<- solve} # create setinverse function
  getinverse <- function() { i }              # create getinverse function
  list(set = set,                             # create a list containing functions created
       get = get,                             # above and the list is returned
       setinverse = setinverse,
       getinverse = getinverse)
}





## This function computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {                   # check if i not null i.e. matrix has not changed and
    message("getting cached data")    # inverse has already been calculated
    return(i)                         # return the cached inverse and end function 
  }
  data <- x$get()                     # program branches here when i is null 
  i <- solve(data, ...)               # calculate inverse of the matrix here 
  x$setinverse(i)                     # set inverse of object
  i                                   # return recently calculated inverse
}

