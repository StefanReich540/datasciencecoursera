## Put comments here that give an overall description of what your
## functions do
## functions of Coursera Data Science: R programming week 3 assignment, 2017-05-05

## Write a short comment describing this function
## this function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  get<- function() x
  }
  setinv<- function(inverse) inv<<-inverse
  getinv<- function() inv
  list(get = get, set = set,
       getinv = getinv,
       setinv = setinv)
}


## Write a short comment describing this function
## this function computes the inverse of the matrix returned by makeCacheMatrix above.
## if the inverse is already been calculated, then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("inverse is cached")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setinv(inv)
  inv
}

