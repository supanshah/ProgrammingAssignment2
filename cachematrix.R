## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix function takes the square matrix as an input.
## set the value of matrix, set function
## get the value of matrix, get function
## set the value of inverse matrix, setmatrix
## get the value of inverse matrix, getinverse

makeCacheMatrix <- function(x = matrix(data1,row,col)) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix ,
             getinverse = getinverse )
	
}


## Write a short comment describing this function
## First check whether inverse has already been calculated for the given matrix, getinverse
## if yes gets the cached data and skips the computation
## Otherwise calculate the inverse of matrix via solve function and cache via setmatrix function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
