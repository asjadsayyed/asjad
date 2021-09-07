makecachematrix <- function(x=matrix()){
  inv <- NULL
  set <- function(y){
    x <<- NULL
    
  }
  get <- function(){x}
  setinverse <- function(invesrse){inv <<- invesrse}
  getiverse <- function(){inv}
  list(set=set, get=get, setinverse=setinverse, getiverse=getiverse)
}

cachesolve <- function(x, ......){
  inv <- x$getinverse()
    if(!is.null(inv)){
      message("getting cache data")
      return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}
