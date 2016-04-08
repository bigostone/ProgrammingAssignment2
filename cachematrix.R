## makeCacheMatrix This function creates a special "matrix" object that can cache its inverse.
## create a structure to store the inverse and create the function to mamamanage the cache
## retutn a function list to
## set= set the value of the matrix
## get = get the value of the matrix
## setinverse = set the value of the inverse
## getinverse = get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL # reset initial values
  #  create the structure
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  #get the matrix
  get <- function() x
  
  #calculate and store in cache
  setinverse<- function(inverse) cache <<-inverse
  #get from cache
  getinverse <- function() cache
  
  # return the function created
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## cacheSolve This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
##For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cache <- x$getinverse() # try to get inverse from cache using getinverse function
  if (!is.null(cache)) {
    # result not nul so cache is valorized
    message("getting cached data")
    #return result
    return(cache)
  } else {
    #cache not present
    #solve
    cache <- solve(x$get())
    # store matrix in cache using setinvevrse function
    x$setinverse(cache)
    #return result
    return(cache)
  }
}
