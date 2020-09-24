## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The functions mentioned below allow us to make a matrix, set the value, get the value, set the inverse and get the inverse of the matrix 
## makeCacheMatrix consists of functions: set,get,setInverse,getInverse
makeCacheMatrix <- function(x = matrix()) {
     inv<-NULL
     set<-function(y){
         x <<-y
         inv<<-NULL
     }
     get<-function(){x}
     setInverse<-function(inverse){inv<<-inverse}
     getInverse<-function(){inv}
     list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
## This function is used to obtain the cache data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv<-x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
       }
       mat<-x$get()
       inv<-solve(mat,...)
       x$setInverse(inv)
       inv
}
