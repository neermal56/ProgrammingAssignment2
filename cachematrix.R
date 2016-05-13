## There are a total of 2 functions. 
## 1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. codes are described below
## i is variable denoting inverse and is set to NULL i.e. i<- NULL
##set <- function(y) {x <<- y; i<<- NULL}. x is set to y and i is assigned NULL.
##get <- function() x returns the vector, x
##setInverse <- function(inverse) i <<- inverse sets the inverse, i, to inverse
##getInverse <- function() i returns the inverse, i
##list(set = set, get = get,setInverse =
##setInverse,getInverse = getInverse) returns the 'special vector' containing all of the functions just defined


makeCacheMatrix <- function(x = matrix()) {
i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function
##  2) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been ##calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##solve(data,...) is the function to calculate inverse of invertible matrix.

cacheSolve <- function(x, ...) {
       
		i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)   
  x$setInverse(i)
  i
}
