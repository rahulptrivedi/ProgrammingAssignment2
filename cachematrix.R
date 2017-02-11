## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly

## Note : For this assignment, it is assumed that the matrix supplied is always
## invertible.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # im ( stands for inverse matrix )is for storing inverse matrix of x
  im <- NULL
  
  # x_im_calc is for storing data for which inverse was produced
  x_im_calc <- NULL
  
  #function to set value of matrix x
  set <- function(y) {
    x <<- y
    #im <<- solve(y)
  }
  
  #function to get value of matrix x
  get <- function()
    x
  
  #function to set value of im i.e inverse matrix
  setInverse <- function(inverseM, data) {
    im <<- inverseM
    
    #captures matrix for which inverseM was calculated
    x_im_calc <<- data
  }
  
  #function to fetch value of im
  getInverse <- function() {
    # condition to check if origional matrix used for calculation for inverse has changed
    # if it has changed then cached data should be flushed
    if (!is.null(im) && x != x_im_calc)
    {
      #message("Matrix has changed")
      im <- NULL
    }
    
    im
  }
  
  # return list
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## cacheSolve: This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getInverse()
  
  # Condition to check if  im has value if yes then we can use it directly instead
  # of recalculation
  if (!is.null(im)) {
    message("getting cached data")
    return(im)
    
  }
  
  
  # since Inverse value does not exists this code calculates inverse
  # and stores it in cache
  data <- x$get()
  im <- solve(data, ...)
  x$setInverse(im, data)
  im
  
}
