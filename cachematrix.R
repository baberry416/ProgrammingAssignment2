## r-prog-012 R Programming. Assignment 2 brian.a.berry@protonmail.ch
# Purpose -- use the scoping rules and the <<- operator in R to cache the result of a 
#       matrix inversion operation and re-use the result from the cache.
#
#       Two functions: makeCacheMatrix and cacheSolve are based on the example and the prototype
#
#       Tests used are included at the end of the file but are commented out so they won't break 
#       a test script. 


# makeCacheMatrix -- This creates  functions to set/get matrix and to set/get inversion.
# these are placed in a list by name.
#
# (the assumption is that the matrix is square and contains real numerics, not complex.
# so the "solve" function can be used.)

# variables and their scopes - xmatrix and xinvert are in the makeCacheMatrix environment.
# they are preserved between calls to set/get functions.
#
makeCacheMatrix <- function(x = numeric() ) {
        xinvert <- NULL        # place the xinvert and xmatrix in the this environment                
        xmatrix <- x
        
        setmatrix <- function (y) {
                xmatrix <<- y          # store these in the environment above
                xinvert <<- NULL
        }
        getmatrix <- function () xmatrix
        setinvert <- function (inv) xinvert <<- inv
        getinvert <- function() xinvert
        
        list (setmatrix = setmatrix,   # list the names for the functions
              getmatrix = getmatrix,
              setinvert = setinvert,
              getinvert = getinvert)
}


## cacheSolve takes a matrix as input and returns the invert.
#       If the inversion has already been calculated, the cached solution is returned
#       Otherwise the matrix is fetched, the inversion is calculated and the result cached and returned.  

cacheSolve <- function() {
        work_invert <- x$getinvert ()     # work_matrix and work_invert are in this functions environment
        if (!is.null (work_invert)) {
                message ("getting solution from cache")
                return (work_invert)
        }
                
        message ("inverting matrix and saving in cache" )
        work_matrix <- x$getmatrix()
        work_invert <- solve (work_matrix)   
        x$setinvert(work_invert)
                
        return (work_invert)
}


#### test the functions makeCacheMatrix and cacheSolve above #####
# these are suitable for executing at the command line or walking through with debugger.
# commented out so they won't break a test script. 

# message ("setup a trivial matrix")
# a <- cbind (1:2, 9:8)
# a

# message ("initialize functions and holding locations")
# x <- makeCacheMatrix (a)

# message (" test set and get of matrix")
# x$setmatrix (a)
# x$getmatrix ()      #visual compare to matrix a

# message ("solve the inverse for the first time")
# cacheSolve ()

# message ("solve the inverse the second time")
# cacheSolve ()      #visual check for messages 

# message ("test getinvert and that the matrix * invert gives a unitary matrix")
# x$getmatrix() %*% x$getinvert()      # visual check