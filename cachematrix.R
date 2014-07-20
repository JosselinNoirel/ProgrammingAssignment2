# Josselin Noirel - Sun Jul 20 16:00:48 CEST 2014

#################################################################
#
# In this R script, we define two functions
#
#	(1) makeCacheMatrix()
#
#	(2) cacheSolve()
#
# allowing a user easily to cache or retrieve the inverse of a matrix.
#
#################################################################

#################################################################
# makeCacheMatrix()
#
# makeCacheMatrix() creates de tailored matrix object that effectively
# (1) stores an internal copy of the actual matrix and (2) makes room for a
# cached version of the inverse of that matrix.  Upon initialisation
# the inverse is set to NULL.
#
# makeCacheMatrix() returns a matrix object allowing the interaction
# with the internal matrix/cached inverse through a set of four functions
#
#	(1) set.internal.matrix(...) - sets the internal matrix
#
#	(2) get.internal.matrix() - gets the internal matrix
#
#	(3) set.inverse(...) - sets the inverse
#
#	(4) get.interse() - gets the inverse
#
# Typical call:
#
#	matrix <- makeCacheMatrix() # assignment has to take place later
#
#	matrix <- makeCacheMatrix(some.data) # immediate assignment

makeCacheMatrix <- function(M=matrix()) {
  inverse <- NULL # This will in time contain the inverse

  # We define four functions allowing the user to interact
  # with the internal matrix and the cached inverse.
  # Names are self-explanatory.
 
  set.internal.matrix <- function (m) {
    M <<- m
    inverse <<- NULL
  }
  
  get.internal.matrix <- function () {
    M
  }
  
  set.inverse <- function (m) {
    inverse <<- m
  }
  
  get.inverse <- function () {
    inverse
  }

  # The matrix object is returned as a list of those four functions

  list(set.internal.matrix=set.internal.matrix,
       get.internal.matrix=get.internal.matrix,
       set.inverse=set.inverse,
       get.inverse=get.inverse)
}

#################################################################
# cacheSolve()
#
# cacheSolve() interrogates a matrix object so as to extract the
# the inverse of the matrix when the inverse has been cached.
# When the cache is uninitialised (NULL), the inverse is calculated and cached
#
# Typical call:
#
#	cacheSolve(matrix)

cacheSolve <- function(mtrx, ...) {
  # Read the cached inverse from the matrix object

  inverse <- mtrx$get.inverse()

  # If the cached inverse hasn't been calculated, calculate it

  if (is.null(inverse)) {
    message("Calculating")
    M <- mtrx$get.internal.matrix()
    inverse <- solve(M, ...)
    mtrx$set.inverse(inverse)
  }

  # Return the inverse matrix

  inverse
}
