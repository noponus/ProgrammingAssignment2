##makeCacheMatrix: This function creates a 
##special "matrix" object that can cache its inverse.
makeCacheMatrix <- function (x = matrix()){
	m <- NULL
	set <- function(y){
	x<<-y
	m<<- NULL
}
get <-function() x
setInverse <-function (solve) m <<- solve
getInverse <-function() m
list (set = set, get = get,
	setInverse = setInverse,
	getInverse = getInverse)
##cacheSolve: This function computes the inverse of the 
##special "matrix" returned by makeCacheMatrix 
cacheSolve <- function(x, ...){
	m <- x$getInverse()
	if(!is.null(m)){
	message("getting cached data")
	return (m)
	 }
        data <- x$get()					#obtain the matrix used
        m <- solve(data, ...)			#calculate the matrix inverse
        x$setInverse(m) 				#inverse of the matrix is stored by Cachematrix set function 
}