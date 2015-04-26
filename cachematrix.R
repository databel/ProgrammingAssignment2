## makeCacheMatrix function 
## This function creates a matrix and solve its inverse

makeCacheMatrix <- function(x = matrix()) {
m<-NULL

# Set the value of the matrix
set <- function(y) {
  x <<- y
  m <<- NULL
}

# get the value of the matrix
get <- function() x

# set the inverse of the matrix
setinverse <- function(solve) m <<- solve


# get the inverse of the matrix
getinverse <- function() m
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)

}

##
##
## This is a second function, called cacheSolve
## This function checks if the inverse matrix is stored in the 
## cache. If it is not in the cache, then it will solve the inverse 
## of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
