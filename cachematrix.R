## Set the input x as a matrix
## Then set the solved value "inver" as a null
## Finally I changed every reference to "mean" to "solve"

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("getting cached matrix inverse")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinverse(inver)
  inver
}