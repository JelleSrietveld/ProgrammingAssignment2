## This function creates a special "matrix" object that can cache the inverse of the matrix
## to save time on this usually costly operation

## function creating the makeCacheMatrix object 

makeCacheMatrix <- function(x = matrix()) {
        Inverse_m <- NULL
  
        set <- function(y) {
                x <<- y
                Inverse_m <<- NULL
  }
        get <- function() x
        setInverse_m <- function(Inverse) Inverse_m <<- Inverse
        getInverse_m <- function() Inverse_m
                list(set = set, 
                get = get,
                setInverse_m = setInverse_m,
                getInverse_m = getInverse_m)
}


## Function that calculates the inverse of the given matrix with Solve function
## First checks if the inverse has been calculated 
## - if so returning the inverse from cached data
## - if not calculating the inverse and setting the value of the inverse in cache via setInverse_m

  cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
        Inverse_m <- x$getInverse_m()
        if(!is.null(Inverse_m)) {
        message("getting cached data")
        return(Inverse_m)
  }
  data <- x$get()
  Inverse_m <- solve(data, ...)
  x$setInverse_m(Inverse_m)
  Inverse_m
}
