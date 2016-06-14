## Put comments here that give an overall description of what your
## functions do

## Creates a matrix object that is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i=NULL
  set=function(m){
    x<<-m
    inv<<-NULL
  }
  get<- function() x
  setInverse<-function(inv)i<<-inv
  getInverse<-function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## find the inverse of the matrix object made by makeCacheMatrix. Gets the inverse from the cache if the inverse is already computed

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (is.null(inv)==FALSE) {
    message("getting cached data")
    return(inv)
  }
  mt <- x$get()
  inv <- solve(mt, ...)
  x$setInverse(inv)
  return(inv)
}
