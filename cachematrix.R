## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# STEP 1: The first function, makeCacheMatrix creates a special "Matrix", 
# which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the Inverse
## 4.get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL               # Changing m to I as "inverse" and setting it to NULL
        set <- function(y){
                x <<- y
                I <<- NULL
        }
        get <- function() x     
        setInverse <- function(solve) I <<- solve  # Setting inverse of x with "solve" function by replacing "mean"
        getInverse <- function() I
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

# STEP 2: The following function calculates the Inverse of the special "vector" created with the above function. 
# However, it first checks to see if the mean has already been calculated. 
# If so, it gets the mean from the cache and skips the computation. 
# Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        I <- x$getInverse()
        if(!is.null(I)){
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setInverse(I)
        I 
}