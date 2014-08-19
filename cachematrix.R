## These functions take advantage of the scopint rules to create a special
## matrix object able to cache its inverse to avoid recomputing it


## makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL   ##initializing the inverse
    
    ##set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ##return the value of the matrix
    get <- function() x
    
    ##set the value of the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    ##return the inverse
    getinverse <- function() inv
    
    
    list(set = set, get = get,  
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of the special "matrix" just defined.
## Returns the cached inverse if it has already been calculated, otherwise,
## calculates it and stores the value in cache.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()  ##check the stored inv to check if already calculated
    if(!is.null(m)) {  
        message("getting cached data")
        return(m)
    }
    data <- x$get()   
    m <- solve(data)
    x$setinverse(m)
    m
    
}
