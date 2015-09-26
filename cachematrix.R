## Functions that cahce the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <-function(y)
  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list 
  (
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Calculate the inverse of the matrix created above. Return a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) 
{
  i <- x$getinverse()
  if(!is.null(i))
  {
    message("calculating data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}
