## Create a special object that stores a matrix 
## and puts its inverse in the cache when first computed


## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, 
             setinv = setinv, 
             getinv = getinv)
}


## Computes the inverse of a special matrix from makeCacheMatrix
## or retrieves the inverse from the cache if already calculated

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        m <- x$get()
        inv <- solve(m)
        x$setinv(inv)
        inv
}
