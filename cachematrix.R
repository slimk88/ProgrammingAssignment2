## The two functions below are used to create a special object that stores a matrix 
## and then cache the inverse of that specific matrix. 
## If the matrix inverse was cached, it is only retrieved by the second function
## otherwise it is calculated.

## The first function called "makeCacheMatrix", creates a list of functions that
## set the value of the matrix, get the value of the matrix, set the inverse of
## that matrix and finally get the value of the inverse matrix

makeCacheMatrix <- function(mtx = matrix()) {
        
        ## "inv_mtx", The inverse of the matrix "mtx", is first initialized to NULL
        
                inv.mtx <- NULL
                
        ## The follwoing 4 functions are created  to (1) set, (2) get the value 
        ## of the matrix "mtx and (3) set and (4) get its inverse. 
                
                set.mtx <- function(y) {
                        mtx <<- y
                        inv.mtx <<- NULL
                } 
                
                get.mtx <- function() mtx
                set.inv <- function(solve) mtx <<- solve
                get.inv <- function() inv.mtx
                
        ## Here the 4 functions are listed via the function list()
        ## These functions will be used in the function "cacheSolve" below
                
                list(set.mtx = set.mtx, get.mtx = get.mtx, set.inv = set.inv, get.inv = get.inv)
        }
}


## This second function, called "cacheSolve", is used to retrieve the cached inverse matrix of "mtx".
## In case the inverse is not found, "cacheSolve" calculate the inverse of "mtx" and returns it.

cacheSolve <- function(mtx, ...) {
        
        ## If the cached inverse matrix is found, the inverse of the matrix is retrieved 
        ## and returned with no need of re-calculating
        
                inv.mtx <- mtx$get.inv()
                if(!is.null(inv.mtx)) {
                message("Please wait while we get the cached data")
                return(inv.mtx)
        }
        
        ## When the inverse of the matrix "mtx" is not found, 
        ## it is calculated through the function solve() below and returned
        
                data <- mtx$get.mtx()
                inv.mtx <- solve(data, ...)
                mtx$set.inv(inv.mtx)
        
                inv.mtx
}
