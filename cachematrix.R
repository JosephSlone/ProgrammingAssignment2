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


# Creates a special object like matrix that is really a matrix containing
# functions that
#
# 1. sets the value of the matrix
# 2. gets the value of the matrix
# 3. sets the inverse of the vector
# 4. gets the inverse of the vector
#

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL  # Creates the variable "m" in the functions local environment
    set <- function(y) {
        # sets the matrix (x) and nulls out it's inverse
        x <<- y
        m <<- NULL
    }
    get <- function() x # gets the main vector
    setsolve <- function(solve) m <<- solve  # solves the vector and stores it
                                             # in the root environment
    getsolve <- function() m  # gets the solved (inverted) matrix from the root
                              # environment

    # list forces makeCacheMatrix to return all of the embedded functions

    list (set = set, get = get,
          getsolve = getsolve,
          setsolve = setsolve)
}


## retrieve the inverted (solved) matrix from the root environment

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    m <- x$getsolve()  # call getsolve function from the pseudo object
    if (!is.null(m)) {  # print a status message and return the
                        # inverted matrix
        message("getting cached data")
        return(m)
    }

    # otherwise invert the matrix, push it up to the
    # root environment in m and return the inverted
    # matrix to the caller.

    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    return(m)  # because I like returns!
}
