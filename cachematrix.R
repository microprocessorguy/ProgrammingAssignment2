###############################
## Krish Sundaresan
## 25-JAN-2015
###############################
# makeCachematrix() -- Creates a list containing fuctions to get/set the matrix and its inverse using solve()
# cacheSolve() -- Calculates the inverse of a matrix using makeCachematrix() by using the cached values as appopriate
#
# Example usage:
#
# > mat <- matrix(rnorm(1024),nrow=32,ncol=32,byrow=TRUE)
# > cachemat <- makeCacheMatrix(mat)
# > cachemat$get() 
# > cacheSolve(cachemat)
# > cacheSolve(cachemat)
# getting cached data
# [,1]        [,2]        [,3]        [,4]        [,5]         [,6]
# [1,]  0.037167890  0.20516323  0.01652198  0.01438210  0.01231357 -0.172866788


## Creates a list containing functions to get/set the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    myinv <- NULL  # variable to hold inverse
    
    # set function for matrix
    set <- function(y) {
        x <<- y
        myinv <<- NULL
    }
    
    # get function for matrix
    get <- function() x
    
    # set func for the inverse
    setmatinv <- function(inverse) myinv <<- inverse
    # get func for the inverse
    getmatinv <- function() myinv
    
    # list to return
    list(set = set, get = get,
         setmatinv = setmatinv,
         getmatinv = getmatinv)  
}


## Calculates the inverse of a matrix using makeCachematrix() by using the cached values as appopriate
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    myinv <- x$getmatinv()
    # Return the value if the inverse is already available
    if(!is.null(myinv)) {
        message("getting cached data")
        return(myinv)
    }
    # else calculate it using solve()
    data <- x$get()
    myinv <- solve(data, ...)
    
    x$setmatinv(myinv)  # cache inverse for future
    myinv  # return the inverse
}
