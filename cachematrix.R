## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix receives a matrix variable, and sets variables and functions in memory,  
## and returns a list of functions nested within makeCacheMatrix. 

makeCacheMatrix <- function(x = matrix()) {
  pm <- NULL                                          ## Initialize pm with NULL
  set <- function(y) {                                ## Create set function to store the matrix passed in the call as x and NULL as pm, both in cache.   
    cache_x <<- y                                     ## Put the initial matrix from the command line into cache as cache_x 
    cache_m <<- NULL                                  ## Initialize caache_m to NULL 
  } 
  get <- function() cache_x                           ## Create function to get/return the matrix passed in the command line call to '$set    
  set_cache_m <- function(pm) cache_m <<- pm          ## Create function to set the value of cache_m in cache to the value of local_m
  get_cache_m <- function() cache_m                   ## Create function to retrieve value of cache_m from cache and return cache_m to the caller and check null    
  list(set = set, get = get, 
       set_cache_m = set_cache_m, 
       get_cache_m = get_cache_m) 
}


## Write a short comment describing this function
## cacheSolve function receives a variable that is a matrix that is expected to have been defined as makeCacheMatrix(), 
## as in m <- makeCacheMatrix(), and then populated with an invertible matrix using the m$set() function that is nested  
## in makeCacheMatrix(). In this syntax, the variable "m" can be any letter. j See validation instruction, above. 


## cacheSolve returns the inverted form of the submitted matrix. 
## When cacheSolve is called, cacheSolve checks to see if there already exists a non-NULL value for m in cache.  
## If cacheSolve finds a non-NULL value for m existing in cache already, it returns that value.   
## If cacheSolve does not find an existing non-NULL value for m in cache, cacheSolve gets the commandline values for m, inverts the matrix  
## in m, and sets the value of m in the cache environment to the just-computed inverted matrix. 
## cacheSolve then evaluates the ending matrix so as to return it. 

cacheSolve <- function(x) {                  ## Receive value of makeCacheMatrix from the caller function    
  pm<- x$get_cache_m()                       ## Get pm in the cache environment and set in pm. 
  if(!is.null(pm)) {                         ## Check to see if pm is NULL.  
    message("getting cached data")           ## If pm is not NULL, return the value of pm with a message.
    return(pm) 
  }                                          ## If we get to this line, pm was NULL 
  startingmatrix <- x$get()                  ## Call function x$get in makeCacheMatrix to obtain the inverted matrix assign it to startingmatrix. 
  endingmatrix <- solve(startingmatrix)      ## Use solve() to invert the startingmatrix.  Assign the result to endingmatrix. 
  x$set_cache_m(endingmatrix)                ## x$set_cache_m() in makeCacheMatrix to set pm in the cache environment to the local non-NULL inverted result matrix
  endingmatrix                               ## Evaluate endingmatrix so as to return it to caller/console if 
} 