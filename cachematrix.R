## Put comments here that give an overall description of what your
## functions do

## The function below creates a special kind of matrix whose inverse can be
## cached after being computed. 

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
        x <<- y
        inv <- NULL
      }
      get <- function()
        x
      setinverse <- function(inverse)
        inv <<- inverse
      getinverse <- function()
        inv
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function below accesses the cached inverse of the special matrix created
## with the function above, or computes the inverse if there is nothing cached.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)){
        message("getting cached inverse")
        return(inv) ## Return a matrix that is the CACHED inverse of 'x'
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      return(inv) ## Return a matrix that is the COMPUTED inverse of 'x'
}
