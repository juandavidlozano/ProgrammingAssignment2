## Juan Lozano Assigment 2 for coursera 
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {   ##The first thing that occurs in the function is the initialization of two objects, x and m
  m <- NULL
  set <- function(y) {  ## When set() is executed, it does two things: Assign the input argument to the x object in the parent environment, and
    x <<- y             ## Assign the value of NULL to the m object in the parent environment.
    m <<- NULL          ##  This line of code clears any value of m that had been cached by a prior execution of cacheSolve().
  }
  get <- function() x   ## makeCacheMatrix() defines the getter for the vector x.
  setInverse <- function(solve) m <<- solve   ## makeCacheMatrix() defines the setter for the mean m.
  getInverse <- function() m    ## makeCacheMatrix() defines the getter for the mean m
  list(set = set, get = get,   ## this  code assigns each of these functions as an element within a list(), 
                                ## and returns it to the parent environment.
       setInverse = setInverse,
       getInverse = getInverse)
}


##  cacheSolve() required to populate or retrieve the mean from an object of type makeVector().

cacheSolve <- function(x, ...) {  ##Like makeCacheMatrix(), cacheSolve() starts with a single argument, x
  m <- x$getInverse()  ## the function attempts to retrieve a solve from the object passed in as the argument. First, it calls the getInverse() function on the input objec
  if(!is.null(m)) {   ##Then it checks to see whether the result is NULL
    message("getting cached data")  ##If the result of !is.null(m) is FALSE, cacheSolve() gets the matrix from the input object, calculates a solve(),
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)  ##then returns the value of the solve() to the parent environment by printing the solve object
  x$setInverse(m)
  m
}
