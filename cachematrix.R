## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Here we have makeCacheMatrix which is a special Matrix object that 
## allows us to cache (store) the inverse of the matrix along with the 
## matrix itself. This will save memory if we have to compute it 
## multiple times 
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
		x <<- y
		## if we change the matrix, we have to wipe the inverse
		i <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) i <<- solve
	getInverse <- function() i
	list(set=set, get=get, setInverse=setInverse, 
getInverse=getInverse)
}


## Write a short comment describing this function

## cacheSolve it the function that we call to get the inverse matrix. 
## if it has already been called then our special matrix already has a 
## value set, and getInverse is not null and would return the inverse
cacheSolve <- function(x, ...) {
	i <- x$getInverse()
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i<- solve(data,...)
	x$setInverse(i)
        ## Return a matrix that is the inverse of 'x'
	i
}
