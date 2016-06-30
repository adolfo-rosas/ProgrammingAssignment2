## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix takes a matrix : x, assumed inversible
## defines a variable i, uses it to keep an interim value in the list environment 
## returns a list of 4 functions get, set, getinv, setinv

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

## cacheSolve takes a list of functions, assumed to contain at least: get,getinv,setinv
## checks interim inverse value stored in list environment calling getinv()
## returns it if not NULL, otherwise retrieves main value from list environment with get()
## inverts it, sets interim inverse with setinv(), returns inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
