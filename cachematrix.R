## Caches the inverse of the input matrix

## Creates a special "matrix" object that caches the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        ## Set the original matrix
        get <- function() x
        ## Get the original input matrix
        setInv <- function(solve) inv <<- solve
        ## Calculates the inverse of the input matrix and caches it
        getInv <- function() inv
        ## Get the inverse matrix
        list(set = set, get = get, 
             setInv = setInv,
             getInv = getInv)
}


## Computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        ## Get the inverse from the cache
        if(!is.null(inv)) {
                message("getting the cached result")
                return(inv)
        }
        ## If the inverse was calculated, returns the cache
        data <- x$get()
        inv <- solve(data, ...)
        ## If the cache was empty, calculate the inverse
        x$setInv(inv)
        ## Save the result into the cache
        inv
               
        ## Return a matrix that is the inverse of 'x'
}
