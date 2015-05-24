## This is the solution to the 2nd assignment in the R-programming course. It is
## on use of scoping to cache a computation result (inverse of a matrix)

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  
    
    set <- function(y){    #set changes the matrix created by this function
        x <<- y            # use of <<- to guaranteee the variable is globaly
        inv <<- NULL       # changed. inv needs to be changed with the matrix
    }
    get <- function() x    # simply spit out the value of the matrix
    setinverse <- function(inverse) inv <<- inverse # assign a value as 
                                                    # the inverse
    getinverse <- function() inv   # spit out the inverse of the matrix
    list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
    # output: a list of 4 functions for setting (changing) and getting (reading)
    # the matrix and its inverse
}


## cacheSolve: computes the inverse of the special "matrix" returned 
## by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse() # see  inverse of x, by accessing this item in its list
    if (!is.null(inv))  { # if inv is already calcualted, display message, and
        message('Using cached data') # and returen the value. cacheSolve will 
        return(inv)                  # exit at this point
    }
    
    data <- x$get()  # if the inverse was never calculated, the matrix itself is
    inv=solve(data)  # read out, then its inverse calculated, and finally, 
    x$setinverse(inv) # the proper entry is set for the matrix.
    inv  # The inverse is the value returned by this function
}
