## makeCacheMatrix creates a list which actually is the function to
## 1.set 2.get 3.setginverse 4.getinverse

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}
## Returns a matrix that is the inverse of 'x'
#First checks if the matrix inverse is already computed. If so, it retrieves 
#it from cache. Else, computes the inverse of the matrix and stores in cache.

  cacheSolve <- function(x, ...) {
    i <- x$getmatrixinverse()
    if (!is.null(i)) {
      message("getting cached matrix inverse")
      return (i)
    }
    matrix <- x$get()
    i <- solve(matrix) 
    x$setmatrixinverse(i)
    i
  }

