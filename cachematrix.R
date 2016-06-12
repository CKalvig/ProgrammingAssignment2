## MY functions are similar to the example functions. The first will create a list of functions. 
## You can store the first result in a variable, and when you call that variable in the second function it will create the inverse matrix, unless
## it alreadt exists, then will cache it. You can test this by calling cacheSolve twice with the same argument
## Example usage:
## x<-matrix(rnorm(9),3,3) to create a square matrix
## x1 <- makeCacheMatrix(x)
## cacheSolve(x1) this will create an inverse matrix, non-cached.
## cacheSolve(x1) call it again to see the Caching message pop up, since the inverse matrix data already exists.


##  Just like the example, creates a list of functions that are called by the second main function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <-function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Also basically the same as the example function, just using solve instead of mean. Will check for existence of inv first.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
