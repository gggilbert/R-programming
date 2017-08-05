## I will write two functions, one is to get the matrix, the other is to acquire the inverse matrix

## Evaluate the matrix and get it

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
  
## get the inverse of x

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    if(identical(data,solve(data,...))){
      messge('inverse is the same')
      return(data)
    }
    m <- solve(data,...)
    x$setinverse(m)
    m
  }
