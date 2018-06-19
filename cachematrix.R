## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix will create a cache of a matrix.
# e.g., m <- makeCacheMatrix(matrix(c(1,2,3,4), 2, 2))
# The caching version of the matrix can be passed to the cacheSolve function
# so that the inverse of the matrix is only computed once.
# The cache can be updated by calling the set function(
# e.g., m.set(matrix(c(4,3,2,1), 2, 2))
# 
# The inverse of the cached matrix is obtained by calling cacheSolve function
# e.g., result <- cacheSolve(m)

## Write a short comment describing this function
## Creates a cached matrix object of the supplied matrix that caches the inverse
makeCacheMatrix <- function(x = matrix())
{
  inverse <- NULL
  set <- function(y)
  {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  
  return (list(set=set, get=get, setInverse=setInverse, getInverse=getInverse))
}


## Write a short comment describing this function
## Computes the inverse of the supplied matrix cache, by using the cache to avoid recomputing the inverse
cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
  inverse = x$getInverse()
  if (!is.null(inverse)) return (inverse)
  
  result = solve(x$get(), ...)
  x$setInverse(result)
  
  return (result)
}
