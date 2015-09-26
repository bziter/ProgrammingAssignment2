## This file has two functions: "makeCacheMatrix" and "cacheSolve"
## They are used together to calculate and store the inverse of a matrix.
## The purpose of these functions is to potentially save computation time 
## by checking to see if the inverse of a matrix is already stored. 
## If it isn't, the inverse is calculated.


## Below is the first function, "makeCacheMatrix".
## This function is meant to be called with a squared matrix as its argument.
## It sets up a list with four objects: $set, $get, $setinv, and $getinv
## The square matrix is stored in $get

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(mtrx) {
    x <<- mtrx
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invrs) inv <<- invrs
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Below is the second function, "cacheSolve"
## It checks $getinv from "makeCacheMatrix" 
## to see if an inverse has already been calculated.
## If not, it retrieves the matrix from $get, 
## then calculates the inverse and stores it in $setinv,
## at which point, it can be retrieved from $getinv.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


# I referenced the following resources while completing this assignment:
# - The assignment description and example code on the Coursera course site
# - This introduction to Git/GitHub by Hadley Wickham: http://r-pkgs.had.co.nz/git.html
# - The YouTube series "Introduction to Git and GitHub" by Data School