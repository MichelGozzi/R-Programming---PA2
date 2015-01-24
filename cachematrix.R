## The operators <<- and ->> are normally only used in functions, and cause a search to be made through parent
## environments for an existing definition of the variable being assigned. If such a variable is found
## (and its binding is not locked) then its value is redefined, otherwise assignment takes place in the global environment.

## This function creates a NULL length matrix that has the method necessary to cache the inverse of the input matrix
## in a global variable. 

makeCacheMatrix <- function(GlobInvMx = matrix() ) { 
    flag <- NULL    
    
    setMx <- function( InputMx ) {   
        GlobInvMx <<- InputMx
        flag <<- NULL
    }
    
    getMx <- function() GlobInvMx
    
    setInvMx <- function(InvMx) flag <<- InvMx
    
    getInvMx <- function() flag
    list(setMx = setMx, getMx = getMx,
         setInvMx = setInvMx,
         getInvMx = getInvMx)
}


## cacheSolve function gets a matrix as input and verify if it has changed since the last inverse calculation. If it has not, it retrieves
## the inverse of the input matrix from the cache and returns it's vaule. If the input matrix has changed or there is no inverse matrix value in cache, it computes the inverse, save it to the global variable and returns the inverse matrix

cacheSolve <- function(InputMx, ...) {
    Mx <- InputMx$getmean()
    
    if(!is.null(Mx)) {
        message("getting global cached data")
        return(Mx)
    }
    
    temp <- InputMx$get()
    Mx <- solve(temp, ...)
    InputMx$setInvMx(Mx)
    Mx
}
