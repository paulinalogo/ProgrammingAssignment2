## I will write two functions in order to cache the inverse of a matrix in case it has already been computed

## This function stores four different functions that allow to 
# 1. set  the matrix
# 2. get the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	  inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function checks, through the functions stored in makeCacheMatrix, if the inverse of a given matrix was already computed
# If it was, it returns that inverse matrix that was already stored, if not, it computes the inverse of a given matrix. 

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
