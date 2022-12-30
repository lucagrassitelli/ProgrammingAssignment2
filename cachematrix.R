## This function takes as input a matrix and returns a list with 4 functions, 
#i.e. a function setting the value of the matrix,
#a function that provides the matrix,
#a function that sets the result of the solve(), which is the inverted matrix
#a function that provides the inverted matrix

makeCacheMatrix <- function(x = matrix(, nrow = 2)) {
  inverse_matrix <- NULL
  
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  
  get <- function () x
  
  setinverse <- function(solve) inverse_matrix <<- solve
  
  getinverse <- function() inverse_matrix
  
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function takes in input the previous list, checks if the 
#inverted matrix has already been calculated and stored in memory, and if not,
# calculates it, stores it in memory, and returns the inverted matrix.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getinverse()
  
  if (!is.null(inverse_matrix)){
    print("already in cache")
    return(inverse_matrix)
  }
  
  dataofmatrix <- x$get()
  
  inverse_matrix <- solve(dataofmatrix)
  
  x$setinverse(inverse_matrix)
  return(inverse_matrix)  
}

