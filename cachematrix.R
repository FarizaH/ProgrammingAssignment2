## This is Assigment 2 - Module

## This is makeCacheMatrix function
## It creates a special "matrix" object that can cache its inverse
## The object is called invs

makeCacheMatrix <- function(x = matrix()) {
    invs <- NULL
    set <- function(y) {
      x <<- y
      invs <<- NULL
    }
    get <- function() x
    setinvs <- function(inverse) invs <<- inverse
    getinvs <- function() invs
    list(set = set,
         get = get,
         setinvs = setinvs,
         getinvs = getinvs) 
}


## This function will return a matrix that is the inverse of x
## This is to retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    
    invs <- x$getinvs()
    if(!is.null(invs)) {
      message("getting cached data")
      return(invs)
    }
##   Returns original matrix    
    data <- x$get()
    invs <- solve(data, ...)
    x$setinvs(invs)
    invs
}
