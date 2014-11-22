## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix receives a matrix variable, and sets variables and functions in memory,  
## and returns a list of functions nested within makeCacheMatrix. 

makeCacheMatrix <- function(x = matrix()) {
  pm <- NULL                                          
  set <- function(y) {                                   
    cache_x <<- y                                  
    cache_m <<- NULL                               
  } 
  get <- function() cache_x                              
  set_cache_m <- function(pm) cache_m <<- pm    
  get_cache_m <- function() cache_m                       
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

cacheSolve <- function(x) {                      
  pm<- x$get_cache_m()               
  if(!is.null(pm)) {                    
    message("getting cached data")  
    return(pm) 
  }                                       
  startingmatrix <- x$get()             
  endingmatrix <- solve(startingmatrix)   
  x$set_cache_m(endingmatrix)            
  endingmatrix                            
} 