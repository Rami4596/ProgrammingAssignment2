# 1- makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        set_Inverse <- function(inverse) inv <<- inverse
        get_Inverse <- function() inv
        
        list(set = set, get = get, 
             set_Inverse = set_Inverse,
             get_Inverse = get_Inverse)
}

# 2- cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_Inverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_Inverse(inv)
        inv
}

X <- matrix(1:4,2,2)
print(X)
myMatrix_object <- makeCacheMatrix(X)
cacheSolve(myMatrix_object)