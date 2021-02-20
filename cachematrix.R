## The functions "cacheSolve" compute the inverse of a matrix and output to the
## to the console through the function "makeCacheMatrix"

makeCacheMatrix <- function(A = matrix()) {
  invMat <- NULL  # Set the container empty
  setValue <- function(z) {
    A <<- z       # Assign matrix to Z
    invMat <<- NULL   # Set the container empty
  }
  # Define function and set it value to get 
  get <- function() A
  # Define function to set the inverse. 
  setinverse <- function(inverse) invMat <<-inverse 
  # Return a list with the above four functionsinverse
  # Define function to get the inverse
  getinverse <- function() invMat
  list(setValue=setValue, get=get, setinverse=setinverse, getinverse=getinverse)
}
# The function below returns the inverse of the matrix A. It uses if condition to
# verify if the inverse has already been computed. if it is, it will use the results for 
# computation. Otherwise, it computes the inverse, assign the value in the cache via
# "setinverse" function.
cacheSolve <- function(A, ...) {
  invMat <- A$getinverse() # This gives the cached value for the inverse
  if(!is.null(invMat)) {  # If the cache was not empty,it returns it
    message("getting cached data.")
    return(invMat)
  } 
  # The cache was empty. We need to calculate it, cache it, and then return it.
  MdataValue <- A$get() # Get value of matrix
  invMat <- solve(MdataValue) # Calculate inverse
  A$setinverse(invMat)  # Cache the result
  invMat                    # Return the inverse
}
