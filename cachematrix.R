## Matrix inversion could be computationally costly and storing the cache of its inverse can save 
##   computing resources.
## makeCacheMatrix() function creates a special "matrix" object that can cache its inverse.
## We'll assume that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
 
    cache <- NULL
    set <- function(y) { 
      x <<- y
      cache <<- NULL
    }
    get <- function() x 
    setCacheMatrix <- function(solve) cache <<- solve    
    getCacheMatrix <- function()cache
    list(set=set,get=get,
         setCacheMatrix=setCacheMatrix,
         getCacheMatrix=getCacheMatrix)
}


##This function returns the inverse of a matrix. It first checks to see if there is a cached copy of the inverse.
##If there is, it uses the cached copy thereby saving time in computing. If cache is NULL then it computes the inverse using solve() function

cacheSolve <- function(x, ...) {
       
  
    cache <- x$getCacheMatrix()
    if(!is.null(cache)){
      message("retrieving from cache")
      return (cache)
    }
    data <-x$get()
    cache <- solve(data)
    x$setCacheMatrix(cache)
    cache
}

