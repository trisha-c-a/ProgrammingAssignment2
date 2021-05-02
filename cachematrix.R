## makeCachematrix creates an matrix object that can cache its own inverse
makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  stinverse <- function(inverse) a <<- inverse
  gtinverse <- function() a
  list(set = set,
       get = get,
       stinverse = stinverse,
       gtinverse = gtinverse)
  
}


## cacheSolve computes inverse of the matrix returned by the function above
cacheSolve <- function(x, ...) {
  a <- x$gtinverse()
  if (!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$stinverse(a)
  a
}
