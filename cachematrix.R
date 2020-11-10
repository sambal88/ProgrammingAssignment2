## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {


## This creates a special matrix object that can cache its inverse
  
  makeCacheMatrix <- function(x = matrix()) { ## defines the argument using default mode of "matrix"
    a <- NULL                             ## set 'a' as NULL that holds value of matrix inverse 
    set <- function(y) {                    ## defines the set function to assign new value of matrix in parent environment 
      x <<- y                         
      a <<- NULL                        ## reset a to NULL if there is a new matrix
    }
    get <- function() x                     ## get function is defined and it has value of the matrix argument
    setinverse <- function(inverse) a <<- inverse  ## assigns 'a' value
    getinverse <- function() a                     ## gets the value of 'a' when called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ##to refer to the functions with the $ operator
    
  }
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## This function returns a matrix that is the inverse of 'x'i.e. the makeCacheMatrix above
  #cacheSolve will retrieve the inverse from the cache if the inverse has already been calculated
  
    a <- x$getInverse()
    if(!is.null(a)){
      message("getting cached data")
      return(a)
    }
    b <- x$get()
    a <- solve(b,...)
    x$setInverse(a)
    a
  }

