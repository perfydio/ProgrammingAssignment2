## Here is two functions: the first function create a special matrix and 
## its inverse; the second function gets the inverse from cache and skips
## the computation. Otherwise, it calculates the inverse of the matrix  
## and sets the value of the inverse in the cache via the setsolve function

## This function create the special matrix which is really a list
## containing a funtion to: 
## 1: set the value of the matrix; 2: get the value of the matrix;
## 3: set the value of the inverse;4: get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function calculates the inverse and set it in the cache

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached matrix")
                return(inv)
        }
        data <- x$get()
        m <- mean(matrix, ...)
        x$setsolve(inv)
        inv
}
