## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## test inverse of matrix

##m <- matrix(rnorm(24),4,4)
##m1 <- makeCacheMatrix(m)
##cacheSolve(m1)

##[,1]       [,2]       [,3]       [,4]
##[1,] -1.7415355  0.9834088  1.5344314  1.2478426
##[2,]  2.7723795 -1.3773760 -1.7447666 -2.0029871
##[3,] -0.5650599  0.7457904  0.5345695  0.8425379
##[4,] -0.7629184  0.4867587  0.9861724  1.2874457
