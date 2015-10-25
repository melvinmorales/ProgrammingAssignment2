# Author: Melvin Morales
# The first function creates a matrix which contains a function to
# set the matrix
# get the matrix
# set the inverse of the matrix
# get the inverse of the matrix
        
makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                # Set the matrix
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                
                # Get the matrix
                get <- function(){
                        x
                }
                
                # Set to the cache "solve"
                setmtxin <- function(solve = matrix() ){
                        m <<- solve
                }  
                
                # Get the value m if this has been assigned
                getmtxin <- function(){
                        m
                } 
                list(set = set, get = get,
                     setmtxin = setmtxin,
                     getmtxin = getmtxin)    
}

# This function returns de inverse of the matrix
# If the inverse had been calculated, then uses the cached data
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmtxin()
        
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        
        mtxin <- x$get()
        m <- solve(mtxin, ...)
        x$setmtxin(m)
        m
}
