##  This program has 2 functions together can be used to compute inverse of matrix
##  Since matrix inverse is an expensive operation, this can be used to cache
##  the inverse matrix and return if without recomputing it.
##  Usage:
##     source("cachematrix.R")
##     a <- makeCacheMatric();
##     a$set(matrix(4:7,2,2))
##     cacheSolve(a)
## 

## makeCacheMatrix:  
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  mCache <- NULL
  
  ## create matrix
  set <- function(y) {
    x <<- y
    mCache <<-NULL
    
  }
  ## get a matrix
  get <- function() x
  
  ## cache inverse matrix
  setMatrix <- function(mInverse) mCache <<- mInverse
  
  ##get inverse matrix
  getInverse <<- function() mCache
  
  ## create a list of functions.
  list (set = set, 
        get = get, 
        setMatrix = setMatrix, 
        getInverse = getInverse)
}


## cacheSolve: 
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
   mCache <- x$getInverse();
   
   ## if exists retrun cached inverse matrix
   if (!is.null(mCache))
   {
     message('Returning data from the cache')
     return(mCache)
   }
   message(' Matrix does not exist in cache, create a new and storing in cache')
   ## create new matrix
   matrix <- x$get()
   ## use solve function to create inverse matrix
   cache <- solve(matrix, ...)
   ## save inverse matrix
   x$setMatrix(cache)
   return(cache)
}
