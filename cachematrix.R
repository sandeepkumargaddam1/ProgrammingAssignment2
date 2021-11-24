#Thefunction makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:
#set and get the value of the matrix
#set and get the value of the inverse

#inverse returned after computation 
makeCacheMatrix <- function(x = matrix()) {
         i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}



#icacheSolve should retrieve the inverse from the cache matrix returned from makeCacheMatrix
cacheSolve <- function(x, ...) {
         i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
        
}
