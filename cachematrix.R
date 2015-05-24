## This is the solution to the 2nd assignment in the R-programming course. It is
## on use of scoping to cache a computation result (inverse of a matrix)

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}


## cacheSolve: computes the inverse of the special "matrix" returned 
## by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv))  {
        message('Using cached data')
        return(inv)
    }
    
    data <- x$get()
    inv=solve(data)
    x$setinverse(inv)
    inv
}
