##The makeCacheMatrix function creates and gets the inverse of the matrix 
##and the cacheSolve function checks if the inverse has been cached, 
##if yes, it returns the cached matrix, if no, it computes the value

##This function creates the inverse of the matrix 
##it is assumed that the input matrix is invertible
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
        x <<- y
        i <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {i <<- inverse}
  getInverse <- function() {i}
  list(set = set, get = get, setInverse = setInverse, 
       getInverse = getInverse)
}

##This function first checks if the inverse has already been calculated,
##if yes, the value is obtained from cache, else the value is calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  matrixval <- x$get()
  i <- solve(matrixval,...)
  x$setInverse(i)
  i
}

###Testing the code
##source("cacheMatrix.R")
##> inputmatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
##> inputmatrix$get()
##      [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> inputmatrix$getInverse()
##NULL
##> cacheSolve(inputmatrix)
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> inputmatrix$getInverse()
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
