## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        invert <- NULL
        
        setmatrix <- function (y)
                x <<- y
                invert <<- NULL
        getmatrix <- function () x
        setinvert <- function (inv) invert <<- inv
        getinvert <- function() invert
        
        list (setmatrix = setmatrix
              getmatrix = getmatrix
              setinvert = setinvert
              getinvert = getinvert)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        invert <- x$getinvert ()
        if (!is.null (invert)) {
                message ("getting solution from cache")
                return (invert)
        }
                
        message ("inverting matrix and saving in cache" )
        matrix <- x$getmatrix()
        invert <- solve (matrix, ...)
        x$setinvert (invert)
                
        return (invert)
}
