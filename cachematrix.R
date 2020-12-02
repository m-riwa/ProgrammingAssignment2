## makeCacheMatrix chaches the inverse of a matrix
## cacheSolve computes the inverse of the matrix or gets the cached value if it already exists


## This function creates a special “matrix” object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #set the inverse i to NULL
    set <- function(y){ #set the value of the matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x  #get the value of the matrix
    setinverse <- function(inverse) i <<- inverse  #set the value of the inverse
    getinverse <- function() inv  #get the value of the inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function ccomputes the inverse of the special “matrix” returned by makeCacheMatrix
## if the value is already calculated, it's retrieved from the cache

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){   #if inv already exists, get the cached value
        message('getting cached data...')
        return(inv)
    }
    data = x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}
