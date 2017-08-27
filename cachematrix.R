## Put comments here that give an overall description of what your
## functions do
## 1. set -- set the value of the matrix
## 2. get -- Get the value of the matrix
## 3. setinverse -- set the value of the inverse of the matrix
## 4. getinverse -- get the value of the inverse of the matrix

## Write a short comment describing this function
##This function creates a special matrix, that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    
    getinverse <- function() inv
    
    list(set= set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special matrix. If the inverse has
## been calculated or set it returns the value from cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <-x$get()
    
    inv <- solve(data,...)
    
    x$setinverse(inv)
    
    inv
}
