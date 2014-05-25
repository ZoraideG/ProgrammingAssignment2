## This pair function allows to skip the calculation of 
##a matrix inverse if this is already been calculated into the same 
##environment. 
## the first function creates a list of function containing the matrix input and the 
##previously calculated inverse. 


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(invers) inv <<- invers
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## the following function checks whether the inverse of the given matrix
## already exists. If it exists, it returns the inverse matrix, 
##otherwise it performs the calculation. 

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        else{
                print("calculating inverse")
                data <- x$get()
                inv <- solve(data, ...)
                x$setinv(inv)       
        }
        
        inv
}


