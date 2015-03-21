###################################################
# Functions to Compute and Cache a Matrix Inverse #
###################################################

# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(inv) m <<- inv
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


# The following function calculates the inverse of the special matrix
# created with the above makeCacheMatrix(). However, it first checks to see if the
# inverse has already been calculated. If so, it gets the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setinv`
# function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached matrix inverse")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}




# an example where the inverse has been calculated #
set.seed(1985)
r<-matrix(rnorm(9),nrow=3,ncol=3)
rr<-makeCacheMatrix(r)
rr$get() # the matrix #
rr$setinv(solve(r)) # setting the inverse #
rr$getinv() # retrieving the inverse #
cacheSolve(rr) # using the cached matrix function #

# an example where the inverse has not been calculated #
y<-matrix(rnorm(4),nrow=2,ncol=2)
yy<-makeCacheMatrix(y)
cacheSolve(yy)
solve(y)

