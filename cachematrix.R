## Set of functions to cache inverse of a matrix
## The functions make use of lexical scoping mechanism

## makeCacheMatrix() creates an R object that stores
## a matrix and its inverse

makeCacheMatrix <- function( x = matrix() ) {
	inv <- NULL # initialize the inverse

	# define setter and getter for the matrix
	set <- function( y ) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x

	# define setter and getter for the inverse
	setinv <- function( inverse ) inv <<- inverse
	getinv <- function() inv

	# return setters and getters to the parent environment
	list( set = set, get = get,
		setinv = setinv,
		getinv = getinv )
}

## cacheSolve() requires an argument returned by
## makeCacheMatrix() to retrieve the inverse from the
## cached value that is stored in the makeCacheMatrix()
## object's environment

cacheSolve <- function( x, ... ) {
	## If inverse of the matrix is alreday cached, return it
	inv <- x$getinv()
	if( !is.null( inv ) ) {
		message( "getting cached data" )
		return( inv )
	}

	## If inverse of the matrix is not yet cached, compute
	## and set it
	data <- x$get()
	inv <- solve( data, ... )
	x$setinv( inv )
	inv
}