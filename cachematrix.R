## makeCacheMatrix function could creat a "matrix" object and the object will 
## be reserved for the cacheSolve function to call. For the first time the cacheSolve function
## is called, the function use the stored matrix to calculate out the inverse of matrix
## and store the value to "m". If the inverse has been calculated,the function gets the value
##from the cache and skips the computation.

## To creat a "matrix" object.

makeCacheMatrix <- function(x = matrix()) {
        m<- NULL
        setmatrix <- function(y){
                x <<- y
                m <<- NULL
        }
        getmatrix <- function(){x}
        setinversion <- function(solve) {m <<- solve}
        getinversion <- function() {m}
        list(setmatrix = setmatrix, 
             getmatrix = getmatrix,
             setinversion = setinversion,
             getinversion =getinversion)
}


## To calculate the inverse of the matrix or print the value from cach. 

cacheSolve <- function(x, ...) {
        m <- x$getinversion()
        if (!is.null(m)){
                message("getting cached data")
                return(m)
        }
        m <- solve(x$getmatrix(), ...)
        x$setinversion(m)
        m
}
