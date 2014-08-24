## FUNCTIONS FOR CACHING THE INVERSE OF A MATRIX

## ********
## Summary:
## ********

## In the cases where the inverse of a matrix needs to be computed repeatedly,
## caching the inverse matrix reduces the computational load.
## The two functions below - 'makeCacheMatrix' and 'cacheSolve' - together
## allow caching of the inverse of a square invertible matrix.


## **************************
## Caching the inverse matrix
## **************************

## This function serves as the cache of the calculated inverse matrix. 
## It has 4 key features:
## SET: Updates the matrix in the cache with the new values of the matrix.
##      (Inverse matrix is reset to empty when the matrix object is updated.)
## GET: Returns the current values of the matrix in the cache to the calling function.
## SET_INVERSE: Updates the inverse matrix with the newly computed values.
## GET_INVERSE: Returns the current values of the inverse matrix in the cache.

makeCacheMatrix <- function(x = matrix()) {
        
        # Initialize the inverse matrix as an empty matrix.
        matrix_inverse <- matrix()
        
        # SET: Update the matrix object in the cache with the new values.
        set <- function(y) {
                x <<- y
                
                # Reset the inverse matrix in the cache to an empty matrix.
                matrix_inverse <- matrix()
        }
        
        # GET: Return the cached value of the matrix object to the calling function.
        get <- function() x
        
        # SET_INVERSE: Update the inverse matrix with the newly computed values.
        set_inverse <- function(new_inverse) matrix_inverse <<- new_inverse
        
        # GET_INVERSE: Return the cached value of the inverse matrix to the 
        # calling function.
        get_inverse <- function() matrix_inverse
        
        # List containing the set, get, set_inverse, and get_inverse functions.
        list(
                set = set,
                get = get,
                set_inverse = set_inverse,
                get_inverse = get_inverse                
                )
}



## ********************************************
## Calculating and returning the inverse matrix
## ********************************************

## This function first checks whether the inverse matrix is empty in the cache.
## If the inverse matrix is not empty, the function fetches the current values from cache.
## If the inverse matrix is empty, it proceeds to perform the following:
##
## 1. Obtain the new values of the matrix object from cache
## 2. Compute the inverse of the matrix
## 3. Update cache with the newly computed inverse matrix.

cacheSolve <- function(x, ...) {
        
        # Fetch the current value of the inverse matrix from cache
        matrix_inverse <- x$get_inverse()
        
        # Check if the inverse matrix is empty in cache.
        if(nrow(matrix_inverse) == 1 & ncol(matrix_inverse) == 1 & is.na(matrix_inverse)) {

                # If empty, fetch the new values of the invertible matrix from cache.
                matrix <- x$get()
                
                # Calculate the new inverse of the matrix.
                matrix_inverse <- solve(matrix, ...)
                
                # Update the cache with the newly computed inverse matrix.
                x$set_inverse(matrix_inverse)
                
                # Return the values of the newly computed inverse matrix
                return(matrix_inverse)
                
        } else {
                
                # If the inverse matrix in cache contains non-NA values, fetch the inverse matrix
                message("Fetching inverse matrix from cache")
                
                # Return the values of the inverse matrix
                return(matrix_inverse)
        }
        
        # Return the values of the inverse matrix
        matrix_inverse
        
}
