##for caching first we make a matrix which can store the required cache data to corresponding computations
makeMatrix <- function(x = matrix()){
  m <- NULL     ## with beginning of function x is declared matrix and m is nullified to clear any previously declared value
  set <- function(y){
    x <<- y 
    m <<- NULL
  }
  get <- function() x ## As x is not part of get environment, x is part of parent environmrnt.as of lexical scopping x is argument submitted
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set,get = get, ## r auttomatically set the final default command as output
       setSolve = setSolve,
       getSolve = getSolve)
}
##cacheMatrix function takes only makeMatrix as argument,for any atomic vector x$getSolve is invalid. R gives error
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
 
