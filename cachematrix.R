## Put comments here that give an overall description of what your
## functions do

## The first function "makeCacheMatrix" creates the matrix using the list  of four functions
   ## Sets the value of the matrix
   ## Gets the value of the matrix
   ## Sets the value of the inverse matrix
   ## 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL    # initializes m
        
        ## defines the four functions
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse 
        getinverse <- function() m
        
        ## Creates a list of the four functions
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Cachesolve is used with the makeCacheMatrix function to 
   ## check the matrix if it can be inverted
   ## Returns the cached data if an inverse matrix cannot be returned
   ## Solves the matrix inverse problem

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                m <- x$getinverse()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setinverse(m)
                m
        }
