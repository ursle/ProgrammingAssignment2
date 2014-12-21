## The makeCacheMatrix() function and the cacheSolve() function enable the inverse of a
## matrix to be cached. The output of makeCacheMatrix needs to be stored in a new variable.
## Every time the inverse needs to be retrieved, the cacheSolve() function is called on this
## new variable.

## This function returns a list containing functions that can get and set the matrix,
## as well as get and set the inverse of the matrix.
## This function needs to be run on a matrix whose inverse should be cached.
makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y){
                x <<- y
                im <<- NULL
        }
        get <- function(){
                x
        }
        setinverse <- function(solve){
                im <<- solve
        }
        getinverse <- function(){
                im
        }
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function retrieves the inverse of the cache matrix. If the inverse was calculated previously,
## it simply returns this previously calculated value.
## If no inverse has been calculated yet, this is done and the result is returned.
## The function takes the output of the makeCachematrix() function as the first argument.
cacheSolve <- function(x, ...) {
        
        im <- x$getinverse()
        if(!is.null(im)){
                message("getting cached data")
                return(im)
        }
        mat <- x$get()
        im <- solve(mat, ...)
        x$setinverse(im)
        im
}
