## Done by Charles - hiicharles@gmail.com

## The function makeCacheMatrix will create a special matrix with 5 functions.
## - x is invertible matrix (nonsingular or nondegenerate)
makeCacheMatrix <- function(x = matrix()) {
    
    # Local variable
    mat_data <- x
    inverse <- NULL
    changed <- TRUE

    ## Return data  
    get <- function() {
        mat_data
    }
    
    ## Update mat_data variable with m.
    ## Note: 
    ## - changed variable is updated to TRUE.
    ## - inverse variable not being updated here.  
    set <- function(m) {
        mat_data <<- m
        changed <<- TRUE
    }
    
    ## Return changed.
    ## Note:
    ## - TRUE when matrix (mat_data) is updated but inverse not being updated.
    ## - FALSE when inverse is inverse matrix of current matrix.  
    haschanged <- function() {
        changed
    }
    
    ## Return inverse
    ## Note:
    ## - the value of inverse maybe outdated.  Call haschanged to check if inverse is outdated.
    ## - NULL when inverse not being updated at the first time.
    getinverse <- function() {
        inverse
    }
    
    ## Update the variable inverse with i.
    setinverse <- function(i) {
        inverse <<- i
        changed <<- FALSE 
    }
    
    # Return the special matrix of type list with 5 functions
    list(get = get, set = set, haschanged = haschanged, getinverse = getinverse, setinverse = setinverse)
}


## The cacheSolve function will return a matrix that is the inverse of 'x'.
## x is special matrix (list with 5 functions) created by makeCacheMatrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    changed <- x$haschanged()
    inverse <- x$getinverse()
    
    ## check for cached inverse and it is not outdated.
    ## is.null is FALSE mean has inverse cache
    ## changed is FALSE mean not yet outdated
    if( !is.null(inverse) & !changed ) {
        message("Cache found and still valid. Use cache inverse matrix. Fast approach.")
        
        # Use the cached inverse value.
        return(inverse)
    }
    
    ## Inverse matrix not yet being set or is outdated.
    message("Cache not found or outdated. Compute inverse matrix. Slow approach.")
    
    ## Retrieve matrix from x
    mat_data <- x$get()
    
    # Compute inverse matrix.
    inverse <- solve(mat_data, ...)
    
    # Update inverse in x
    x$setinverse(inverse)
        
    # Return inverse
    inverse
}

