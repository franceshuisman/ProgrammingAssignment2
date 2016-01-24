## These two functions together generate the inverse of a matrix
## and cache this inverse for later use.


## makeCacheMatrix allows for caching of the inverse of a matrix,
## and clears the cache when a new matrix is provided (via the 
## $set function). It does not perform the inverse transformation 
## itself.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve first checks to see if an inverse of the matrix
## is cached, and returns this if possible. If there is no
## cached inverse, cacheSolve generates one and caches it.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
