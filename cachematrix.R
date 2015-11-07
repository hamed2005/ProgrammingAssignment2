## Creating a special matrix and calculating it's inverse using the
## cached inverse when possible.

## makeCacheMatrix creates a special matrix, a list containing functions
## to set/get value of the matrix and also set/get it's inverse.
makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
        set <- function(y) {
				x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of a special matrix using solve()
## function, but if it's present in cache, it returns cached value.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting chached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
