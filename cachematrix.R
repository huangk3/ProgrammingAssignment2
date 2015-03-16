## Put comments here that give an overall description of what your
## functions do
#this script includes the function to cache the inverse of a matrix; 
## Write a short comment describing this function
#this function creates a special matrix object which can cache the inverse;
makeCacheMatrix <- function(x = matrix()) {
#set the inverse equal to NULL;
	s<-NULL
	set<-function (y) {
#the set function assgins the argument to x;
		x<<-y
#reset the inverse to NULL after the set function is called;
		s<<-NULL
	}
	get<-function() x
	setInverse<-function(solve) s<<-solve
	getInverse<-function() s
#create a list of the functions;
	list(set=set, get=get,
		setInverse=setInverse
		getInverse=getInverse)
}


## Write a short comment describing this function
#this function computes the inverse of the "matrix" returned by the makeCacheMatrix; 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
#get the most recent inverse value
	s <- x$getInverse()
#test if the inverse had been calculated, if so, return the inverse; 
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
#calculate the inverse if it wasn't calculated before;
        data <- x$get()
        s <- solve(data, ...)
        x$setInverse(s)
#return the newly calculated inverse value; 
        s
}
