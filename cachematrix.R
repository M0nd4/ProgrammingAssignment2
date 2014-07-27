############################################################################
## 1. makeCacheMatrix: This function creates a special "matrix" object    ##
## that can cache its inverse.                                            ##
## 2. cacheSolve: This function computes the inverse of the special       ##
## "matrix" returned by makeCacheMatrix above. If the inverse has already ##
## been calculated (and the matrix has not changed), then the cachesolve  ##
## should retrieve the inverse from the cache.                            ##
############################################################################

## Following two functions are used to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix())
{
    ## Initialize the inverse matrix value
    cache <- NULL
    
    ## Set the value of the matrix
    setMatrix <- function(newvalue)
        {
            x <<- newvalue
            ## Since the matrix is assigned a new value, flush the cache
            cache <<- NULL
        }
    
    ## getMatrix() returns the stored matrix encapsulated by the object
    getMatrix <- function()
        {
            ## Return the matrix
            x
        }
    
    ## setInverse() saves the inverse of the matrix
    setInverse <- function(auxiliar)
        {
            cache <<- auxiliar
        }
    
    ## getInverse() returns the cached inverse
    getInverse <- function()
        {
            cache ## return the inverse property
        }
    
    ## Return a list of all the above functions. Each named element of
    ## the list is a function
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## This function assumes that the matrix is always invertible.

cacheSolve <- function(y, ...)
{
    ## Return the inverse from the cache if it has been cached already
    inverse <- y$getInverse() 
    ## Just return the inverse if its already set
    if(!is.null(inverse))
        {
            message("getting cached inverse")
            return(inverse)
        }
    ## Calculate the inverse, cache it, and return it
    ## else, we first get the matrix
    data <- y$getMatrix()
    ## And calculate the inverse
    inverse <- solve(data, ...)
    ## Cache the inverse of the matrix
    y$setInverse(inverse)
    ## Return the result
    inverse
}

## Sample run:
## > source("CacheMatrix.R")
## > x = rbind(c(1,2),c(3,4))
## > m = makeCacheMatrix(x)
## > m$getMatrix()
## [,1] [,2]
## [1,]    1    2
## [2,]    3    4
## > cacheSolve(m)
## [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5

