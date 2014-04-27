## This function defines get & set functions for matrix and its 
## inverse matrix and returns these functions as list


makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }  
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get ,
       setinv=setinv,
       getinv=getinv)
}

## cacheSolve function checks if invser of the matrix is already 
## cached. If inverse of the matrix is chached, it reuns chached 
## value, else cumputes the inverse and caches it for future
## references.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinv()
        if(!is.null(i)){
          message("getting cached data")
          return(i)
        }
        i<-solve(x$get())
        x$setinv(i)
        i
}
