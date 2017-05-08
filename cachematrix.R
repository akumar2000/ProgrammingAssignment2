## This function creates the matrix and provides four function
## They are set, get, setInverse and getInverse. 



makeCacheMatrix <- function(x = matrix()) {
	
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setmatrix<-function(solve) m<<- solve
	getmatrix<-function() m
	list(set=set, get=get, setmatrix=setmatrix,	getmatrix=getmatrix)
}


## This function checks the if the inverse of matrix is in cache 
## if there then returns the cached inverse if matrix



cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
		message("getting cached data")
		return(m)
    }
    matrix<-x$get
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}

