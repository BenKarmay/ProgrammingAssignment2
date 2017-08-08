## This is a pair of functions designed cache the inverse of a matrix.
## By creating a special "matrix" object this function can cash its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
}
        get <- function() x
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        getInverse <- function() inv
        list(set = set, get = get, setInverse = getInverse = getInverse) 


## The inverse of the special "matrix" returned by makeCacheMatrix above is computed by this function. 
        

cacheSolve <- function(x, ...) {
        ## A matrix is returned that is the inverse of 'x'
             inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv      
}
        
}
