########################################################################################################
## File:        cachematrix.R
## Author:      T Long
## Date:        9/19/2014
##
## Description: This script contains methods for caching inverted matrices.  
##              makeCacheMatrix returns an object that stores both an original SQUARE matrix and an 
#                 inverted copy of that matrix.
##              cacheSolve performs a lazy calculation of the inverted square matrix (lazy in the sense 
##                that it uses a cached value of the inversion if it exists.)
########################################################################################################
myMatrix <- matrix(
  c(1,3,2,4,6,5,7,8,9), 
  nrow=3, 
  ncol=3
  )
myMatrix
solve(myMatrix)

# Construct the matrix cache object
mcm <- makeCacheMatrix(myMatrix)

# Solve for an inverted matrix (first time will do the solve
cacheSolve(mcm)
# Call solve again and see that the cache is working
cacheSolve(mcm)

########################################################################################################
## Function:  makeCacheMatrix
##
## Purpose:   Construct an object that stores both an original matrix 
##            and a cached value of the inverse of the original matrix.
##
## Usage:     oMatrixCache <- makeCacheMatrix(matrix(c(1,3,2,4), nrow=2, ncol=2))
##
## Author:    T Long
##
## Date:      9/19/2014
########################################################################################################
makeCacheMatrix <- function(mMatrix = matrix()) 
{
  # Define and Initialize the mInverted matrix to NULL.  This will be the cache value once it's calculated.
  mInverted <- NULL 
  
  # Set accessor to store the original value and flush the cache
  set <- function(inMatrix)
  {
    mMatrix <<- inMatrix
    mInverted <<- NULL
  }
  # Get accessor for the original matrix
  get <- function() mMatrix
  
  # Set accessor to store the cache in the mInverted member variable:
  setInv <- function(cacheSolve) mInverted <<- cacheSolve
  
  # Get accessor to return the Inverted matrix (mInverted) from cache
  getInv <- function() mInverted
  
  # Return a list containing the functions we've just defined.
  list(set = set,
       get=get,
       setInv=setInv,
       getInv=getInv
       )
}


########################################################################################################
## Function:  cacheSolve
##
## Purpose:   Computes inverse of a matrix that is stored within an object (list) 
##            Returns cached value of this matrix if the inverse operation has already been performed.
##
## Usage:     oMatrixCache <- cacheSolve(oMC)  # Where oMC <- makeCacheMatrix(...)
##
## Author:    T Long
##
## Date:      9/19/2014
########################################################################################################
cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  mInverted <- x$getInv()
  
  # Check to see if this matrix inverse has been cached:
  if(!is.null(mInverted))
  {
    message("Using cached version of inverted matrix!")
  }
  else
  {
    # Access the original matrix
    data <- x$get()
    # Compute inverse using solve function
    mInverted <- solve(data)
    # Save the 
    x$setInv(mInverted)
  }
  mInverted
}


