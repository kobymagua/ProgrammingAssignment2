## This is a script containing two functions for caching the inverse of a matrix, which happens to be a time consuming computation.


## This function is used to create the matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  env <- NULL
  set <- function(y){
    x <<- y
    env <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {env <<- inverse}
  getInverse <- function() {env}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function computes the inverse of the special matrix returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
      env <- x$getInverse()
      if(!is.null(env)) {
          message("getting cached data")
          return(env)
      }
      my_matrix <- x$get()
      env <- solve(my_matrix, ...)
      x$setInver(env)
      env
}
