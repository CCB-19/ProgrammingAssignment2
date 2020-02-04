## Creating a cached Inverse of a Matrix
## Writing two functions

## First function will create a vector which allows to set the data and it's value
## as well as setting the mean value and retrieving that value

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
              x <<- y
              s <<- NULL
      }
      get <- function()x
      setsolve <- function(solve) s <<- solve()
      getsolve <- function()s
      list(set = set, get = get, 
           setsolve = setsolve,
           getsolve = getsolve)
}


## This function calculates the inverse of the Matrix checking whether such value already exists.
## In case the value is already in cache it retrieves it. Otherwise it will calculate the Inverse of the Matrix.

cacheSolve <- function(x, ...) {
       s <- x["getsolve()"]
       if(!is.null(s)) {
            message("getting cached data")
            return(s)
       }
       data <- x["get()"]
       s <- solve(data, ...)
       x["setsolve(s)"]
       s
}
