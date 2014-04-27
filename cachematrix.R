## This function creates a special "matrix", containing a function to:
## set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse
##  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}




## This function calculates the inverse of the special "matrix" 
## But implements a chaching function which first checks if the inverse has already been calculated.
## If answer is yes, it takes cached matrix.
## Otherwise, it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
 ## check if the inverse of matrix is  cached
  m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
  # If not , calculate matrix
  data <- x$get()
  m <- solve(data, ...)
  # compute the inverse
  x$setsolve(m)
  m
}
