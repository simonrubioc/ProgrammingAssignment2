## This code was written by Simon Rubio for the Practice Assignment/Week 3 of the R Programming course,
## offered by Johns Hopkins University in Coursera. In this code we explore the applied concept of Lexical Scoping.

## This first function creates a "matrix" object and a list of functions that we'll be calling in the
## cacheSolve function. The idea of the 4 functions is to set and get the input matrix and the output matrix
## (inverse) that we'll be finding either by using the solve function or by looking if there's already a stored cache.


makeCacheMatrix <- function(x = matrix()) {
        inverso <- NULL
        set <- function(y) {
                x <<- y
                inverso <<- NULL
        }
        get <- function(){x}
        setinverso <- function(inv) {inverso <<- inv}
        getinverso <- function(){inverso} 
        list(set = set, get = get,
             setinverso = setinverso,
             getinverso = getinverso)
}


## Function takes the list of functions generated in makeCacheMatrix() as output. If there is already a stored 
##object for the getinverso() function,then the screen displays that inverse matrix.If false, then it gets the 
##input matrix and calls the solve function to find the inverse of that matrix, storing it in the cache. If we
##call again cacheSolve() with the same input, then this time the function will print the inverse from cache.

cacheSolve <- function(x, ...) {
        inverso<-x$getinverso()
        if(!is.null(inverso)){
                message("getting cached data")
                return(inverso)
        }
        datos<-x$get()
        inverso <- solve(datos)
        x$setinverso(inverso)
        inverso
        ## Return a matrix that is the inverse of 'x'
}
