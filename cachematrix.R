## cachematrix.R speeds up the process of finding the
## inverse of matrices. It avoids repeated calculations
## by checking to see if they have already been completed
## for a specific matrix, and retrieves that data from
## the cache.


## makeCacheMatrix is a funtion that creates a special
## matrix object and caches its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL        ## initializes i as NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }        ## assigns the input argument to the x 
                 ## object in the parent environment
                 ## also assigns NULL to the i object in
                 ## the parent environment
        
        get <- function() x ## defines getter function
                            ## for the matrix x
        
        setinv <- function(invE) i <<- invE
                        ## defines setter function for 
                        ## the inverse i
        
        getinv <- function() i
                        ## defines the getter funciton
                        ## for the inverse i
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        ## each set/get function above is assigned as an
        ## element within a list, and returned to the
        ## parent environment
}



## cacheSolve computes the inverse of the matrix object
## returned by makeCacheMatrix. If the inverse of this 
## matrix object has been previously computed, then
## cacheSolve retrieves this data from the cache.
cacheSolve <- function(x, ...) {
        i <- x$getinv()  ## attempts to retrieve an inverse
                         ## from the object passed as the
                         ## argument
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }       ## Checks if inverse has been computed. 
                ## if i != NULL, then data is
                ## retrieved from the cache
        
        data <- x$get()  ## retrieves the matrix object
                         ## returned by makeCacheMatrix
        
        i <- solve(data, ...) ## computes the inverse of
                              ## the matrix object
        
        x$setinv(i)     ## caches the matrix object
        
        i        ## Return a matrix that is the inverse of 'x'
}
