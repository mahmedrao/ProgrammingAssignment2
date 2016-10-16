# makeCacheMatrix - computes the inverse of the matrix and puts it in the cacheSolve
# cacheSolve - checks whether matrix exist in the cache or not 

makeCacheMatrix <- function(input = matrix()) {
    inverse <- NULL
	#set the values of the input matrix
    set <- function(y) {
        input <<- y
        inverse <<- NULL
    }
	#get the input matrix
    get <- function() input
	#compute the inverse of the matrix
    setinverse <- function() inverse <<- solve(input)
	#get inverse of the matrix
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


cacheSolve <- function(x) {
    inverse <- x$getinverse()
	#check if the matrix exist or not
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
