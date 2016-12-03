##The functions calculate inverse matrix from input matrix, if it's posible.
##It is important for input to really be a matrix :)
##Both functions are connected, in other words some inputs for the first function are created
##by the second function (input for setinverse) and vice versa ("a" and "m")
##The point is that this mutual bidirectional link allows us to not repeat the
##calculation for the same input, but just retrieve the result from the cache, where it was
##stored when the first calculation was done.

##First function initializes two object "a" and "m". Then it creates four functions, those are
##elements of the list, which is output for the first function and input for the second 
##function. "Set" restarts the "m", when the input "a" is changed. "Get" saves the input matrix
##for the very first calculation of inverse matrix. Although "setinverse" and "getinverse" are
##created now, input for "setinverse" is from the second function (after first calculation), 
##"getinverse" then use "m" created by "setinverse" as input. All functions are elements
##of the list, which is also output of this makeCachceMatrix function.

makeCacheMatrix <- function(a = matrix()){
        m <- NULL
        set <- function(y){
                a <<- y
                m <<- NULL
        }
        get <- function() a
        setinverse <- function(inverse_matrix) m <<- inverse_matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##Second function takes the list from the first function as input and first it looks whether 
##there is stored value in m and if it is, it returns the value with the message, or if it 
##is not, it takes the matrix from first function and calculates inverse matrix.
##Also it uses result as input for the "setinverse" function.
##Finally it prints the result (inverse matrix of a).

cacheSolve <- function(a, ...) {
        m <- a$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- a$get()
        m <- solve(data, ...)
        a$setinverse(m)
        m
}