## R Programming
## Programming Assignment 2
## Create functions that take advantage of lexical scoping in R to calculate and store the inverse of a given matrix

## makeCacheMatrix creates a special object that contains a matrix, stores it and also stores the inverse of that matrix,
## when calculated using the cacheSolve() function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setmatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(
        setmatrix = setmatrix,
        getmatrix = getmatrix,
        setinverse = setinverse,
        getinverse = getinverse
    )
}

## cacheSolve calculates the inverse matrix of a makeCacheMatrix type object and stores it 
## within the makeCacheMatrix environment

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$getmatrix()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
