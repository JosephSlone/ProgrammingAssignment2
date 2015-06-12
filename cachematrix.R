## Put comments here that give an overall description of what your
## functions do

# Programming Notes:
#
#   (FYI - I put lots of comments in my code, mostly for my benefit)
#
#   < From the R manual >
#   The operators <<- and ->> are normally only used in functions,
#   and cause a search to made through parent environments for an
#   existing definition of the variable being assigned. If such a
#   variable is found (and its binding is not locked) then its value
#   is redefined, otherwise assignment takes place in the global
#   environment.


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list (set = set, get = get,
          getsolve = getsolve,
          setsolve = setsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- x$getsolve()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
