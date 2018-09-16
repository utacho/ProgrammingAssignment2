## Put comments here that give an overall description of what your
## functions do

  ## return: a list containing functions to
    ##              1. set the matrix
    ##              2. get the matrix
    ##              3. set the inverse
    ##              4. get the inverse
    ##         this list is used as the input to cacheSolve()
    

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
    set = function(y) {
        # use `<<-` to assign a value to an object in an environment 
        # different from the current environment. 
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
         ## @x: output of makeCacheMatrix()
    ## return: inverse of the original matrix input to makeCacheMatrix()
    
    inv <-  x$getInverse()
    
    # if the inverse has already been calculated
    if (!is.null(inv)){
        # get it from the cache and skips the computation. 
        message("getting cached data")
        return(inv)
    }
    
    # otherwise, calculates the inverse 
    # mat.data = x$get()
    # inv = solve(mat.data, ...)
    
    mat <- x$get()
    inv <- solve(mat, ...)
    
    # sets the value of the inverse in the cache via the setinv function.
    x$setInverse(inv)
    inv
}



# example for testing
#z <- makeCacheMatrix(matrix(c(1,1,4,0,3,1,4,4,0), nrow = 3, ncol = 3))
#x <- cacheSolve(z)
#x

