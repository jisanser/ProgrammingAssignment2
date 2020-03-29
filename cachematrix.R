## Put comments here that give an overall description of what your
## functions do

# The first function (makeCacheMatrix) serves to calculate the inverse of a
# given matrix and store it in an object that can be recalled later to avoid
# doing the same computation. The second function (cacheSolve) can be used to
# either recall the inverse stored in cache or to compute the inverse of a new
# matrix.

# As precised in the assignment, it is assumed that the matrix supplied is
# always invertible.

# ==============================================================================
## Write a short comment describing this function

# This function will always calculate de inverse of the matrix given to it and
# store it in an object in the working environment.

makeCacheMatrix <- function(x = matrix()) {
    # Creates the object "cachematrix" in the working environment (outside the
    # function) which will store the matrix given to the function.
    message("| Attempting to store the matrix...")
    cachematrix <<- x
    # Creates the object "cacheinverse" in the working environment (outside the
    # function) which will store the inverse of the matrix given to the
    # function.
    message("| Attempting to compute its inverse...")
    cacheinverse <<- solve(x)
    message("| No errors encountered.")
}

# ==============================================================================
## Write a short comment describing this function

# This function will return the inverse stored in cache or, if a new matrix is
# given to it, will compute the inverse of the new matrix and store it in cache.

cacheSolve <- function(x = NULL, ...) {
    # Checks if the function has been given a matrix. If no matrix has been
    # given to the function, it will use the matrix stored by the function
    # "makeCacheMatrix" in the object "cachematrix". This allows the user to
    # quickly call on the inverse stored in cache without having to retype the
    # name to the matrix every time.
    if (is.null(x)){
        message("| No matrix given. The matrix stored in cache by 
                \"makeCacheMatrix\" will be used.")
        x <- cachematrix
    }
    # Checks if the matrix given to this function is the same as the one stored
    # in the cache.
    if (identical(x, cachematrix)){
        # If both matrices are the same, it returns the inverse of the matrix
        # that has already been computed by "makeCacheMatrix" and stored in
        # cache "cacheinverse".
        message("| Returning the inverse of the matrix stored in cache:")
        return(cacheinverse)
    }
    # If the matrices are different, it assumes two things: (1) that the user
    # wants to know the inverse of the matrix given to the function
    # "cacheSolve", and (2) that the user wants this new matrix and its inverse
    # to be the ones stored in cache.
    else {
        message("| New matrix given. Attempting to compute its inverse and 
                store it in cache...")
        cachematrix <<- x
        cacheinverse <<- solve(cachematrix)
        message("| No errors encountered.")
        message("| Returning the inverse of the new matrix:")
        return(cacheinverse)
    }
}

# ==============================================================================
# TESTDRIVE
# The following code is just for the purpose of trying out both functions:

# Examples of different matrices:
testmatrix1 <- matrix(1:4, nrow = 2, ncol = 2) # Valid matrix 1.
testmatrix2 <- matrix(3:6, 2, 2) # Valid matrix 2.
testmatrix3 <- matrix(1:9, 3, 3) # Matrix without inverse.
testmatrix4 <- matrix(3:8, 2, 3) # Matrix that is not square.

# Calling both functions:
makeCacheMatrix(testmatrix1)
cacheSolve()
cacheSolve(testmatrix1)