## The two functions makeCacheMatrix and cacheSolve
## either calculate and cach an inverse of a matrix
## or retrieved a precached value of the inverse

## The arguement of makeCacheMatrix: the original matrix
## The output of makeCacheMatrix: a list of functions
## The arguement of cacheSolve: the output of makeCacheMatrix
## The output of cacheSolve: the inverse (either precached or calculated)

## USAGE:
## Store a matrix that you want to inverse in an object (e.g. "mat1")
## Store a result of makeCacheMatrix(mat1) in another object (e.g. "cachemat1")
## Pass cachemat1 down the cacheSolve function

## ACKNOWLEDGEMENT:
## The code below is inspired by the example code for the Assignment

## makeCacheMatrix
## This function creates a list of functions to pass values between the environments
## Each function in the list is specified in its own comment
## The arguement x: the original matrix
makeCacheMatrix <- function(x = matrix()) {
      ## clearing the value of the inverse in the working environment
      inv <- NULL
      ## a function that initializes 2 objects in the cache:
      set <- function (y) {
            x <<- y ## a duplicate of the original matrix
            inv <<- NULL ## an empty object to cache the inverse
      }
      ## a function to retrieve the original matrix from the cache
      get <- function() x
      ## a function to pass the inverse from the working environment into the cache
      setinv <- function(i) inv <<- i
      ## a function to retrieve the inverse from the cache
      getinv <- function() inv
      ## an output: a list containing the four functions specified above
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve
## This function either retrieves precacheed inverse of the matrix
## or calculates it and passes it into the cache
## The arguement x: makeCacheMatrix('original matrix')
cacheSolve <- function(x, ...) {
      ## an object retrieveing the inverse from the cache
      inv <- x$getinv()
      ## a check if the inverse is already calculated (i.e. its value is not NULL)
      if(!is.null(inv)) {
            message("getting cached data")
            ## A print of the precached value of the inverse while quitting the function
            return(inv)
      }
      ## An object retrieving the original matrix from the cache
      data <- x$get()
      ## A calculation of the object's (i.e. the matrix) inverse
      inv <- solve(data, ...)
      ## A function passing the calculated inverse to the cache
      x$setinv(inv)
      ## A print of the calculated inverse
      inv
}
