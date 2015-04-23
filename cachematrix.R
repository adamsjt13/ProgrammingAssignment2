## cachematrix.R is a program that is used to calculate the inverse of a matrix
## and place the result in the cache so that it can be accessed at a later time
## without having to recalculate the inverse. 

## makeCacheMatrix is a function that creates a list containing functions 
## for setting and obtaining the matrix to take the inverse as well setting and 
## obtaining the inverse once it is computed

makeCacheMatrix <- function(x = matrix()) {
        matrix_inv <- NULL
        set_matrix <- function(y){
                x <<- y
                matrix_inv <<- NULL
        }
        get_matrix <- function() x
        set_inv <- function(inverted) matrix_inv <<- inverted
        get_inv <- function() matrix_inv
        list(set_matrix = set_matrix, get_matrix = get_matrix, 
             set_inv = set_inv, get_inv = get_inv)
}


## cacheSolve is a function that first checks to see if the inverse of the 
## matrix has been computed. If the inverse has already been computed, the 
## the function prints a message to the console to inform the user that data
## is being obtained from the cache. If the inverse has not yet been computed,
## the function computes and sets the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix_inv <- x$get_inv()
        if(!is.null(matrix_inv)) {
                message("getting cached data")
                return(matrix_inv)
        }
        data <- x$get_matrix()
        matrix_inv <- solve(data, ...)
        x$set_inv(matrix_inv)
        matrix_inv
}
