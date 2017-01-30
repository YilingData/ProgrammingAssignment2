## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
              x<<-y
              inv<<-NULL
        }
        get <- function() x
        setinverse <- function(pinv) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
  
}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        if(!is.null(inv)) {
            message("getting cached matrix")
            return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinverse(inv)
        inv
}
