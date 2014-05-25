##'cacheSolve' function calculates the inverse of the special "matrix" created with the 'makeCacheMatrix' function.

##'makeCacheMatrix' function creates a special "matrix", which is really a list containing a function to
##set the value of the matrix using the 'set' function,
##get the value of the matrix using the 'get' function,
##set the value of the inverse matrix using the 'setInverse' function and
##get the value of the inverse matrix using the 'getInverse' function.

makeCacheMatrix <- function(x = matrix())
{               
        s<-NULL
	set <- function(y)
	{
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) s<<-solve
	getInverse <- function() s
	list(set = set, get = get, 
	     setInverse = setInverse,
	     getInverse = getInverse)
}
##'cacheSolve' function calculates the inverse of the special "matrix" created with the 'makeCacheMatrix' function.
##At first, it checks the determinant of the matrix.
##valueDet is a numeric vector which indicates the determinant of the matrix.
##If determinant is zero, then it displays the message that the determinant is zero and the inverse cannot be computed.
##If determinant is non-zero, it checks if the inverse has already been calculated. 
##in this case, it gets the inverse from the cache and skips the computation.
##Otherwise,it calculates the inverse of the cached data and 
##sets the value of the inverse in the cache via the setInverse function.
cacheSolve <- function(x, ...) 
{
        s <- x$getInverse()
	if(!is.null(s))
	{
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	valueDet<-det(data)
	if(valueDet==0)
	{
		message("Determinant = 0 .i.e. It is a non-singular matrix.\nThus, Inverse of the Matrix cannot be computed.")
	}
	else
	{
		s<- solve(data, ...)
		x$setInverse(s)
		s
	}
}
