makeMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set,get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}
cacheMatrix <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
##eXAMPLE
## >y <- matrix(c(9,1,3,1),nrow = 2)
## > y
##        [,1] [,2]
##  [1,]    9    3
##  [2,]    1    1
## > a <- makeMatrix(y)
## > cacheMatrix(a)
##            [,1] [,2]
##  [1,]  0.1666667 -0.5
##  [2,] -0.1666667  1.5
 
