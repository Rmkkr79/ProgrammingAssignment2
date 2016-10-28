## The two functions "makeCacheMatrix" and "cacheSolve" work in conjunction. 
## The first function "makeCacheMatrix" is used to convert the given matrix into a special object and
## returns a list.
## The second function "cacheSolve", using the special object returned by the "makeCacheMatrix" function, computes
## the inverse of the matrix and caches it in the list created by the first function for later retrievel.

## This function takes the matrix to be inversed and create a list and stores the matrix along with the functions to set and get the result.
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
## This function takes the object (list) returned by the "makeCaheMatrix" function and checks if the inverse is already computed.
## If not, gets the matrix, computes and returns the inverse at the same time it caches it in the list created by "makeCacheMatrix" function for later retrivel.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
      }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
