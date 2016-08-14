## Getting inverse of matrix is time comsuming operation for computers. In some cases, there could be times that
## you need same inverse of a matrix multiple times. To overcome this situation, here are functions to cache an
## inverse of a matrix and return when needed.

# #makeCacheMatrix: This function takes a matrix and returns a list that contains 
#                   set, get , setInverse , getInverse functions inside.

makeCacheMatrix <- function(x = matrix()) {
  InverseOfMatrix <- NULL
  
  set <- function(newMatrix)
  {
    x <<- newMatrix
    InverseOfMatrix <<- NULL
  }
  get <- function()
  {
    x
  }
  
  setInverse <- function (InverseMatrix)
  {
    InverseOfMatrix <<- InverseMatrix
  }
  getInverse <- function()
  {
    InverseOfMatrix
  }
  
  list (set = set , get = get , setInverse = setInverse , getInverse = getInverse)
  
}


## cacheSolve function take a "special" matrix which is created by makeCacheMatrix funtion, actually a list.Firstly, check
##  if there is an inverse of that matrix, if there is an inverse, it will return it. if not , it will create an inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  InverseOfMatrix <- x$getInverse()
  if(!is.null(InverseOfMatrix) )
  {
    message("Inverse data is available. The cached inverse of matrix is:")
    return(InverseOfMatrix)
    
  } 
  data <- x$get()
  InverseOfMatrix <- solve(data, ...)
  x$setInverse(InverseOfMatrix)
  InverseOfMatrix
}

##EXAMPLE:
#ranMatrix <- matrix(rnorm(25),5,5)
#cachedMatrix <- makeCacheMatrix(ranMatrix)
#cacheSolve(cachedMatrix)

