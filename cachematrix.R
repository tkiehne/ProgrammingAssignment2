## R Programming - rprog014 - May 2015
## Assignment #2
##
## These functions provide the means to manage a square numerical matrix with
## caching capabilities.
##
## Note that if I were to do this in reality I would refactor these two functions into 
## one function capable of handling its own computations and cache management, thus
## emulating the encapsulation principle of object-oriented design
##
## Sample usage:
##
## ma <- makeCacheMatrix(matrix(c(1,2,3,4), nrow = 2, ncol = 2)) #creates the cacheable object
## cacheSolve(ma) #computes the inverse and sets the cache
## cacheSolve(ma) #returns the cached value
## ma$set(matrix(c(5,6,7,8), nrow = 2, ncol = 2)) #sets new matrix value and resets cache
## cacheSolve(ma) #computes the new inverse and sets the cache

## makeCacheMatrix - Set a square matrix and provide cached computations
##
## Usage: makeCacheMatrix(x) - create a cacheable matrix, 
##                              where x is a square numeric matrix
##        get()              - get the value of the matrix
##        set(y)             - set the value of the matrix, 
##                              where y is a square numeric matrix
##        getInverse()       - get the inverse of the matrix
##        setInverse(i)      - set the inverse of the matrix, 
##                              where i is the precomputed inverse numerical matrix of x
##
makeCacheMatrix <- function(x = matrix()) {
  #attribute to hold the computed inverse matrix
  inv <- NULL
  
  #methods to get & set attributes
  get <- function() {x}
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  #methods to get & set inverse property
  getInverse <- function() {inv}
  setInverse <- function(i) {
    inv <<- i
  }
  
  #register the methods as a list
  list(set = set, get = get,
       getInverse = getInverse,
       setInverse = setInverse)
}


## cacheSolve - Provide a caching layer for makeCacheMatrix objects
##
## Usage: cacheSolve(x, ...) - get or compute the cached inverse of a 
##                              makeCacheMatrix matrix, where x is a
##                              makeCacheMatrix object containing a square numeric
##                              matrix and ... represents additional parameters to 
##                              pass to the solve() function for computing the inverse
##
cacheSolve <- function(x, ...) {
  #get inverse matrix attribute from object
  inv <- x$getInverse()
  
  #if inverse matrix is set, return it
  if(!is.null(inv)){
    return(inv)
  }
  
  #otherwise, get the matrix, compute the inverse
  inv <- solve(x$get(), ...)
  
  #then set the inverse attribute in the object and return it
  x$setInverse(inv)
  inv
}
