# This pair of functions creates a special "matrix" object that can cache its inverse
# and get the value of the inverse.

## This function creates a special 'matrix' object that can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {  # sets the value of the matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x   # gets the value of the matrix
    setInverse <- function(inverse) inv <<- inverse   # sets the value of the matrix's inverse 
    getInverse <- function() inv    # gets the value of the matrix's inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the matrix created by the 
## makeCacheMatrix function above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cacheSolve 
## function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {  # checks if the inverse has already been calculated
      message('getting cached data')
      return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)  # calculates the value of the matrix's inverse
  x$setInverse(inv)
  inv
}

mat <- matrix(rnorm(9), 3, 3)

test_matrix <- makeCacheMatrix(mat)
test_matrix$get()
test_matrix$getInverse()
cacheSolve(test_matrix)
