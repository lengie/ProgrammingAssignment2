## Examining lexical scoping in R through caching of inverse of a matrix
## For 'R Programming' course in Jan 2015, programming assignment 2
## Written by LEngie, cookiestork@gmail.com

## makeCacheMatrix will create a matrix obj that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL #to keep any preexisting 'm' in the global from being redefined
     set <- function(y){
          x <<- y
          m <<- NULL
     }
     get <- function() x 
     setinverse <- function(solve) m <<- solve #this calls the function solve
     getinverse <- function() m #this gets it from cache
     list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve will compute the inverse of the matrix from makeCacheMatrix and retrieve
##  it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse() #check to see if it is already cached
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(x)
     x$setinverse(m)
     m
}
