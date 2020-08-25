## the makeCacheMatrix function compute a special matrix y registri is datas in cache,
## then with cacheSolve function we returns the inverse of this matrix pass liked 
## argument the special matrix obtenied winth makecacheMatrix function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
         m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinversa <- function(solve) m <<- solve
     getinversa <- function() m
     list(set = set, get = get,
          setsolve = setinversa,
          getsolve = getinversa)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
         m <- x$getsolve()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setsolve(m)
     m
}
