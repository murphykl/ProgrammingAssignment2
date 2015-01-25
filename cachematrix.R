## These two functions allow for the creation and storate of a matrix, 
##the generation and storage of its inverse, and the retrieval of the 
##stored inverse if the stored inverse exists

## This function allows you to create and store a matrix and generate 
##and store the inverse of the created matrix 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(side) {
        l<-rnorm(side^2)
        x<<-matrix(l,side,side)
        m <<- NULL
    }
    get <- function() x
    setinv <- function() m<<- solve(x)
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function allows you to check for a stored inverse matrix in
##the output of makeCacheMatrix, and calculates the inverse if none 
##is stored

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    x$setinv()
    m<-x$getinv()
    m
}
