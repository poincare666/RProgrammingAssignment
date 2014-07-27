## cached version of calculation of inverted matrix
## You can run the following commands to exercise this code
## mm <- makeCacheMatrix(matrix(c(1,2,3, 1,-2,3, 100,2,-100), nrow = 3, ncol = 3, byrow = TRUE, dimnames = list(c("r1", "r2", "r3"), c("C.1", "C.2", "C.3"))))
## cacheSolve(mm)
## mm$get()
## mm$getinverse()
##
## or just
##
## mm <- makeCacheMatrix()
## cacheSolve(mm)
## mm$get()
## mm$getinverse()
##
## Other commands:
## to get the very matrix:	mm$get()
## to get the inverted matrix: 	mm$getinverse()


## Prints for debugging; set debug_logs to 1 if you want debugging messages on the screen
debug_logs <- 0
printD <- function(x) {
       if (debug_logs == 1) {
       	  print(x)
       }
}

## This function implements setter/getter methods for the Matrix Cache - setter/getter will work either for the very matrix or for the inverse of matrix
## it created non-empty inversible matrix if you don't pass a matrix to it
makeCacheMatrix <- function(x  = matrix(c(1,2,3, 1,-2,3, 100,2,-100), nrow = 3, ncol = 3, byrow = TRUE)) {
	m <- NULL
	set <- function(y) {
	    x <<- y
	    m <<- NULL	
	}
	get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function implements the cache of calcs
cacheSolve <- function(x, ...) {
	printD("Here is the matrix to inverse:")
	printD(x$get())

	m <- x$getinverse()
	if(!is.null(m)) {
                message("getting cached data... here the inverted matrix:")
                return(m)
        }
	data <- x$get()
	m <- solve(data)
	x$setinverse(m)
	printD("Here is the inverted matrix:")
	printD(x$getinverse())
	m <- x$getinverse()
}

