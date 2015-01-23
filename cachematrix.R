## Put comments here that give an overall description of what your
## functions do

## makes a cache to store values of a matrix and its inverse.
## outputs a list of functions to access and modify these values.

makeCacheMatrix <- function(matrix = matrix()) {
        
        inv<- NULL #creates cache for inverse.
        
        set <- function(newMatrix){
        matrix <<- newMatrix #caches new matrix
        inv <<- NULL #resets inv
        }
        get <- function() matrix #accesses cached matrix
        getInverse <- function() inv #accesses cached inverse
        setInverse <- function(inverse) inv <<- inverse  #sets cahced inverse
        
        list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
        
}


## Solves for the inverse of a matrix, unless cache stores a value for the inverse.
## '...' contains parameters you want to pass to solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
         inv <- x$getInverse() #accesses cached value of inverse
        
        if(!is.null(inv)){
                message("Getting Cached Data")
                return(inv) #returns cached value if not null
        }
        
        x$setInverse(solve(x$get(),...)) #solves for inverse when there is no cached inverse. 
        x$getInverse()
}
