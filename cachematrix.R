## makeCacheMatrix:  creates a special "matrix" that can cache its inverse
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse<- function(solve) inv_x <<- solve
  getinverse <- function() inv_x
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve:  computes the inverse of the matrix returned by 
## makeCacheMatrix(), unless the inverse has already been calculated, 
## in which case it retrieves it from the cache.

cacheSolve <- function(x, ...) {
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
  }
  inv_x <- solve(x$get())
  x$setinverse(inv_x)
  return(inv_x)
}