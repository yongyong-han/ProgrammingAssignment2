## The functions caching the Inverse of a Matrix
## 

## This function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y){
    x<<- y
    inv<<-NULL
  }
  get<-function() x
  setinvrse<- function(invrse) inv<<-invrse
  getinvrse<- function() inv
  list(set=set, get=get, setinvrse=setinvrse,
       getinvrse=getinvrse)
  
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<-x$getinvrse
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    matx<-x$get()
    inv<-solve(matx,...)
    x$setinvrse(inv)
    inv
  
}
