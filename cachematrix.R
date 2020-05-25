##  RProgramming_wk3_hw
## Caching a matrix inversion result using cods written by Dr.Peng.  Only need to change relative names in Peng's code. 
##The matrix inversion function is "solve". Solve is a existing R function just like "mean".
## Thanks Goodness Dr.Peng wrote the code.   

##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the invers_matrix
##4.get the value of the invers_matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
set <- function(y) {
  x <<- y
  m <<- NULL
}
get <- function() x
setinverse <- function(matrix) m <<- matrix
getinverse <- function() m
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}




## The following function inverse of the special "matrix" created with the above function. 
##However, it first checks to see if the inverse has already been calculated. If so, it gets the mean from the cache 
##and skips the computation. Otherwise, it inverse the matrix and sets the value of the inversed matrix in the cache 
##via the solve function in R.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
