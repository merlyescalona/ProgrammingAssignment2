## Put comments here that give an overall description of what your
## functions do
## 
## This is the code I used to test my functions
## 
## myorg<-matrix(rnorm(100,100,10),10,10)
## mymat<-makeCacheMatrix()
## mymat$set(myorg)
## sol<-cacheSolve(mymat)
## sol
## [1] the inverse matrix

## Write a short comment describing this function
## Comment: Construtor of the special "cache" matrix
##          Has 4 functions, set/get of the orginal matrix
##          plus the setinverse/getinverse to cache.
## Input: the original matrix that will be cached
## Returns: a list with the 4 functions
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function(){ x}
  setinverse <- function(inverse){ 
    m <<- inverse
  }
  getinverse <- function(){m}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## Comment: Verifies if the inverse of the matrix is cached.
##          If its cached, the inverse matrix is returned, otherwise
##          the inverse of the matrix is computed.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <-if(length(x$getinverse())==0){
    print("No cached data. Solving..")
    m <- solve(x$get())
    x$setinverse(m)
    x$getinverse()
  }else{
    print("Getting cached data")
    x$getinverse()
  }
  m
}
