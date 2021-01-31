## These two functions will help to calculate the inverse of a matrix, 
## taking advantage of cached calculations. The first function will create
## a cached matrix, and the second will compute (or retrieve from the cache)
## the inverse of the matrix (we assume it is invertible)

## Create the cached matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_value <- NULL
    set <- function(y) {
      x <<- y
      inv_value <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv_value <<- inverse
    get_inverse <- function() inv_value 
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
  }


## Calculate or retrieve the inverse

cacheSolve <- function(x, ...) {
    inv_value <- x$get_inverse()
    if(!is.null(inv_value)) {
      message("getting cached data")
      return(inv_value)
    }
    data <- x$get()
    inv_value <- solve(data)
    x$set_inverse(inv_value)
    inv_value
  }
