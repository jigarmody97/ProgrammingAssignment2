## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix makes inputs a matrix
## cacheSOlve calculates the matrix inverse if it is not calculated before

## Write a short comment describing this function
## set sets the matrix elements
## get gets the matrix elements
## setinv sets the inverse of the matrix
## getinv gets the matrix inverse if it is already calculated

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y){
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) matinv <<- solve
  getinv <- function() matinv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## checks if the inverse of the matrix is calculated before if not then it
## calculates the inverse of the matrix
 

cacheSolve <- function(x, ...) {
  matinv <- x$getinv()
  if(!is.null(matinv)){
    message("getting cached data")
    return(matinv)
  }
  data <- x$get()
  matinv <- solve(data,...)
  x$setinv(matinv)
  matinv
        ## Return a matrix that is the inverse of 'x'
}
