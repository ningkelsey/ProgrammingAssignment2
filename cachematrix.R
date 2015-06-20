## The two functions are meant to cache the inverse of a Matrix

## makeCacheMatrix function creates a special "matrix" object that 
##   can cache its inverse

makeCacheMatrix <- function(x = matrix()) { # x is the Matrix to be inversed
  i <- NULL # i is to store the result after the inverse process
  set <- function(y) { # this function is setting the value of y to x
    x<<-y
    i<<-NULL
  }
  get <- function() x #return to x
  setinverse <- function(solve) i <<-solve # set the value of inverse to i
  getinverse <- function() i #return to i
  list(set = set, get = get, # function returns to a list with four elements
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" 
##   returned by the first function. If the inverse has already been 
##   calculated, then the cachesolve shouldretreive the inverse from 
##   the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() #call getinverse function in makeCacheMatrix
  if(!is.null(i)){ #if the inverse of the Matrix has been calculated already
    message("getting cached data")
    return(i) #return to result i
  }
  # if inverse has not been calculated before 
  data <- x$get() # get data from x
  i<-solve(data,...) #calculate the inverse
  x$setinverse(i) #store result into x
  i
}
