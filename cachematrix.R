## These two functions can be used in combination to cache a matrix
## and the inverse of that matrix. This allows you to only calculate the 
## inverse of the matrix once and then store it for use in other
## functions or applications. To utilize this function first assign a variable
## to  makeCacheMatrix(x) (i.e. v <- makeCacheMatrix(x)) where x is a matrix (note this function only works with square
## invertable matrices). You can then cache the inverse by by using cacheSolve(v) where 
## v is the variable used to assign to makeCacheMatrix. you can use v$set(new) (where new is a new matrix) to
## cache a matrix. 

## This function is used to convert a matrix into the object format necessary for cacheSolve to
## to cache the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## This function searches for a previously cached version of the matrix and returns that matrix if it exists
## if a previously cached inverse does not exist then it creates the inverse, caches it for later, and returns the inverse
## matrix. It does this by utilizing the makeCacheMatrix function and does this by requiring a variable assigned as
## makeCacheMatrix(matrix) where matrix is any square invertable matrix. 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
