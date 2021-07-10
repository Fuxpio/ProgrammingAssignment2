## Put comments here that give an overall description of what your
## functions do

## The function create a list of functions that can be called to perform set/get on matrix as is and it inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL 
      set <- function(j) {
            x <<- j ##argument passed by invoking makeCacheMatrix
            inv <<- NULL ##when set the matrix it must be new, so inv=NULL to set the cache as cleaned
      }
      get <- function() x ##return argument (matrix) passed to the function
      setinverse <- function(inverse) inv <<- inverse 
      getinverse <- function() inv
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The function call getinverse function. If value is null it means it wasn't calculated yet
## and perform it, otherwise it return the cached value informing the user with a message

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse() ##call the getinverse function over x
      if (!is.null(inv)) { ##check if the inverse has been calculated yet
            message("getting cached data")
            return(inv) ##if so return cached value and exit the function
      }
      data <- x$get() ##if it's not calculated, the inverse is computed
      inv <- solve(data, ...)
      x$setinverse(inv) ##cache the result
      inv #return the inverse matrix
}
