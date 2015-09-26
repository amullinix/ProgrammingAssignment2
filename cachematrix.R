# Matrix inversion is usually a costly computation and 
# there may be some benefit to caching the inverse of a 
# matrix rather than compute it repeatedly
  
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse and to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

# This function calculates the inverse of the matrix created
# with the function from above. It checks if the inverse has 
# already been computed. If it has, then it will reuse the 
# cached result and if not it computes the inverse. 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}

# Return a matrix that is the inverse of 'x'
# x = rbind(c(2, 1), c(1, 2))
# > m = makeCacheMatrix(x)
# > m$get()
#      [,1] [,2]
# [1,]    2    1
# [2,]    1    2

# > cacheSolve(m)
#         [,1]       [,2]
# [1,]  0.6666667 -0.3333333
# [2,] -0.3333333  0.6666667
