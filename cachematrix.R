## Examining lexical scoping in R through caching of inverse of a matrix
## For 'R Programming' course in Jan 2015, programming assignment 2
## Written by LEngie, cookiestork@gmail.com

## makeCacheMatrix will create a obj that can cache a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL #to keep any preexisting 'inv' in the global from being redefined
     set <- function(y){
          x <<- y
          inv <<- NULL #resets inverse if a previous matrix had been calculated
     }
     get <- function() x
     setinverse <- function(inverse) inv <<- inverse #stores solve from cacheSolve
     getinverse <- function() inv #this gets it from cache
     
     #a list output with functions for elements
     list(orig=x,set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve will compute the inverse of the matrix from makeCacheMatrix and retrieve
##  it from the cache
## Inputs: f, makeCacheMatrix, and mat, the matrix to be solved

cacheSolve <- function(f,...) {
     ## Return a matrix that is the inverse of 'x', input to makeCacheMatrix
     inv <- f$getinverse() 
     
     #check to see if it is already cached and if the matrix has changed
     if(!is.null(inv)) {
          #testing whether f$set has changed
          ident <- identical(f$get(),f$orig)
          if(ident==FALSE){
               return("Matrix has changed. Rerun makeCacheMatrix with correct matrix")
          }
          message("Getting cached data")
          return(inv) #will stop here if calculated for the first time
     
     }
     data <- f$get()
     inv <- solve(data)
     f$setinverse(inv)
     #inv
}
