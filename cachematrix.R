## Put comments here that give an overall description of what your
## functions do

## The first function creates R object that stores matrix and its inversed version

makeCacheMatrix <- function(x = matrix()) {
     fun<-NULL
     set<-function(y) {
         x<<-y
         fun<<-NULL
     }
     get<-function() x
     setInverse<-function(inverse) fun<<-inverse
     getInverse<-function() fun 
     list(set=set,get=get,setInverse=setInverse, getInverse=getInverse)
         
}


## The second function uses an argument returned by makeCacheMatrix in order to retrieve 
##the inversed matrix from the cached value that is stored in makeCacheMatrix's environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    fun<-x$getInverse()
    if (!is.null(fun)){
        message("getting cached data!")
    return(fun) }
    matrx<-x$get()
    fun<-solve(matrx, ...)
    x$setInverse(fun)
    fun
    
}



