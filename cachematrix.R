## First function
# this function does
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the inverse matrix
# 4. get the inverse matrix

makeinverseMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse matrix of 'x'. However, it first checks
# to see if the inverse matrix has already been built. If so, it gets the 
#inverse matrix from the cache and skips the computation. Otherwise, it calculates
#the inverse matrix and sets the new inverse matrix in the cache via the setinverse function.
Cacheinverse<- function(x = matrix()) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


### Trials
#set a matrix
mat1<- matrix(1:6, nrow=3, ncol=2)
mat1

mat2<- makeinverseMatrix(mat1)
mat2<- makeinverseMatrix(mat1)
#the second time the code is skipped

