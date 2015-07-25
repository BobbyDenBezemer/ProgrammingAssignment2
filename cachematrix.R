# Function initializes a CacheMatrix object and its methods
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # A set function to set a cacheMatrix object
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # A get function to get a cacheMatrix object
  get <- function() x
  
  # a set function to set the inverse of a cacheMatrix object
  setInv <- function(mat) inv <<- mat
  
  # a get function to get the inverse of a cacheMatrix object
  getInv <- function() inv
  
  # returns the objects methods as a list
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## Return a matrix that is the inverse of a cacheMatrix
cacheSolve <- function(x, ...) {
  # get the inverse of a cache matrix
  inv <- x$getInv()
  
  # if the inverse is already intialized, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if the inverse is not intialized, get the cacheMatrix object
  data <- x$get()
  
  # calculate the inverse of the cacheMatrix object
  inv <- solve(data, ...)
  
  # set the inverse of the cacheMatrix object to the calculated inverse
  x$setInv(inv)
  
  # return the inverse of the cacheMatrix object
  inv
}

## Test example
# initialize a cacheMatrix
testCacheMatrix <- makeCacheMatrix(x = matrix(rnorm(9), nrow = 3, ncol = 3))

# get the cacheMatrix
testCacheMatrix$get()

# calculate the inverse and print it
cacheSolve(testCacheMatrix)

# get the inverse from the cacheMatrix
testCacheMatrix$getInv()
