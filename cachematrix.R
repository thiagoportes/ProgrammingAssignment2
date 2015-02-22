## Creates a Matrix object with special caching functions.
## If the inverse of the Matrix object has already been calculated
## and the object didn't change, the function returns its cached 
## value

## Creates the Matrix object with the special caching functions.
## Is responsable for setting and getting the matrix and its
## inverted value

makeCacheMatrix <- function(x = matrix()) {    
    inversed <- NULL
    
    set <- function(y) {
        x <<- y
        inversed <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inversed <<- inverse
    
    getinverse <- function() inversed
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Checks if the inverse has already been calculated. If so,
## returns the cached value, else calculates, caches and then
## return the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inversed <- x$getinverse()
    
    if(!is.null(inversed)) {
        message('Getting cached data')
        return(inversed)
    }
    
    data <- x$get()
    inversed <- solve(data, ...)
    x$setinverse(inversed)
    
    inversed
}
