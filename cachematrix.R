## cachematrix.r
##
## brainvat
## https://github.com/brainvat/ProgrammingAssignment2
##

#######
##
## makeCacheMatrix (matrix)
##
##    a constructor function that takes a matrix as
##    input and returns an object with ivars for
##    the original matrix as well as its inverse
##
##    methods:
##
##    $get() - getter method for the matrix
##    $set() - setter method for the matrix
##    $getinverse() - getter method for the inverse
##    $setinverse() - setter method for the inverse
##
##
#######

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


#######
##
## cacheSolve (cachematrix)
##
##   returns the inverse of the square invertible
##   matrix created with makeCacheMatrix
##
##
#######

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        ## example
        ##
        ## mx <- makeCacheMatrix(matrix(c(1,-1,1,2), nrow=2, ncol=2))
        ## cacheSolve(mx)
        ## 
        ## 			 [,1]       [,2]
        ## [1,] 0.6666667 -0.3333333
        ## [2,] 0.3333333  0.3333333
        ##
        
        # convert to cachematrix if it is a square matrix 
		cm <- x
		if ((class(x) == "matrix") && (nrow(x) == ncol(x))) {
		        cm <- makeCacheMatrix(x)
		}
        
        # retrieve, calculate, and/or store inverse if this is a cachematrix
		if ((class(cm) == "list") && identical(names(cm), c("set","get","setinverse","getinverse"))) {
                m <- cm$getinverse()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- cm$get()
                m <- solve(data, ...)
                cm$setinverse(m)
                m
		} else {
                message("Usage: cacheSolve(m) where m is a square matrix or cachematrix")
                return(matrix())
		}

}
