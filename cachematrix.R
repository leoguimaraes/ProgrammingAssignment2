## This Functions store a specific cache of matrix and
## calculate the inversed using the solve function

## This function will create and store the cached matrix
## It should be executed before the cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(z) m <<- z
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function that uses the MakeCacheMatrix Function 
## and calculate the solve for the Matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
     if(!is.null(m)) {
       message("getting cached data")
       return(m)
     }
    data <- x$get()
    z <- solve(data)
    x$setinverse(z)
    z
}
## To test the code, you should run:
## a <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
## m <- makeCacheMatrix(a)
## z <- cacheSolve(m)
## z <- cacheSolve(m)
