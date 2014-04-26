## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {       #set the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x       #get the matrix
  setinverse <- function(solve) m <<- solve         #set the inverse of the matrix
  getinverse <- function() m          #get the inverse of the matrix
  list(set = set, get = get,         #output a list
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" created with the above function.

cacheSolve <- function(x, ...) {
m <- x$getinverse()           #the output inverse matrix         
  if(!is.null(m)) {           #if it is calculated before
    message("getting cached data") 
    return(m)                #just return the output which is calculated before
  }
  data <- x$get()             #if it is not calculated before
  m <- solve(data, ...)        #calculate now
  x$setinverse(m)                #save the output 
  m                           #return the output
        ## Return a matrix that is the inverse of 'x'
}
