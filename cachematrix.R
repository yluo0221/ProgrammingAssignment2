## Caching the Inverse of a matrix



makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    #set the value of matrix
    set <- function(y){
        x <<- y
        inverse <<-NULL
    }
    #get the value of matrix
    get <- function() x
    #set the value of inverse
    setinv <- function(inverse) inv <<- inverse
    #get the value of inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    #Try to get the inverse value
    inv <- x$getinv()
    #if the inverse value isn't NULL, return it
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    #otherwise, calculate it and return it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
