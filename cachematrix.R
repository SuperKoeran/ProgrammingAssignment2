
## These two functions below caches the vector and calcuates 
## the certain value using the cached vector 

## This function caches the vector (matrix in this case,) so that could be used in the second function

makeCacheMatrix <- function(x = matrix()) {
    v <- NULL
    set <- function(y) {
        x <<- y
        v <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) v <<- inverse
    getinverse <- function() v
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## This cacheSolve function calculates and returns an inverse matrix of x. 

cacheSolve <- function(x, ...) {
    v <- x$getinverse()
    if(!is.null(v)) {
        message("Getting Cached Data")
        return(v)
        
    }
    data <-x$get()
    v <- solve(data, ...)
    x$setinverse(v)
    v
    ## Return a matrix that is the inverse of 'x'
}

##Below is the testing of the functions created above

m <- matrix(c(1,3,2,4),ncol = 2, nrow = 2)
m
m1 <- makeCacheMatrix(m)
cacheSolve(m1)

