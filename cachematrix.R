## Put comments here that give an overall description of what your
## functions do

## Function which create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

 m <- NULL
 set <- function(y) {
     x <<- y
     m <<- NULL
 }
 get <- function() x
 setinverse <- function(solve) m <<- solve
 getinverse <- function() m
 list(set = set, get = get, 
      setinverse = setinverse, 
      getinverse = getinverse)

}


## Function which finds the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), retrieves the inverse from the cache,
## else computes the inverse

cacheSolve <- function(x, ...) {
 m <- x$getinverse()
 if(!is.null(m)) {
   message("Inverse already computed")
   return(m)
 }
 data <- x$get()
 m <- solve(data, ...)
 x$setinverse(m)
 m
}
