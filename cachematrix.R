##Comments are overly verbose to show understanding, I would typically just provide the class and a brief description of the arguments and return value
##Note that even though there is no class check on the makeCacheMatrix, so any argument can be passed in and stored with the set and get functions, my comments assume a matrix is passed in and works in conjunction with the cacheSolve function

##Create a function that takes an argument (in our case matrix) and returns a list with four functions that can be used on the first argument
##set() sets the value of the matrix
##get() returns the matrix to use for computation
##setinverse() sets the inverse of a matrix so that it is stored in memory and does not need to be recalculated
##getinverse() returns the inverse matrix stored in memory
##we use the <<- operators so that we can reuse the variable outside of the function scope
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL     ##create a new variable inv set it to null
        set <- function(y) {  ##define set as a function that takes an input y
                x <<- y  ## x is assigned to y, allows you to update data
                inv <<- NULL ##resets the inverse whenever a new matrix is set
        }
        get <- function() x  ## assigns get to an empty function that returns x
        setinverse <- function(inverse) inv <<- inverse ## function setinverse assigns inverse to inv
        getinverse <- function() inv ## return inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)  ##return a list of functions
}

##cacheSolve a function that takes an argument x, in our case a list of functions and some matrix value, and returns the inverse of the matrix
## cacheSolve checks to see if there is a inverse in memory and if there is returns the inverse matrix using the getinverse function before calculating the inverse
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()  ##uses the getinverse function to see if an inverse is stored in memory
        if(!is.null(inv)) {
                message("getting cached data.") ##if inverse is exists return message and the inverse matrix
                return(inv)
        }
        data <- x$get()  ##get the matrix from makeCacheMatrix get function
        inv <- solve(data) ##solve the matrix AKA get the inverse
        x$setinverse(inv) ##set the inverse value to so we can call it without recalculating in the future
        inv ##return the inverse matrix
}
