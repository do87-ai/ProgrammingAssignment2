## Matrix inversion is used to create a special object that stores a 
## matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y)  {
                  x <<- y
                  inv <- NULL
          }
          get <- function() x
          setinv <- function(solve) inv <<- solve
          getinv <- function() inv
          list(set=set,get=get,
          setinv=setinv,
          getinv=getinv)
}


## cacheSolve calculates the inverse of matrix x. It first checks to see
## if the inverse matrix has already been calculated. If so, it gets the
## inverse from the cache and skips computation. Otherwise it calculates
## the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getinv()
          if(!is.null(inv)) {
                  message("getting cached data")
                  return(inv)
          }
          data <- x$get()
          inv <- solve(data,...)
          x$setinv(inv)
          inv
}
