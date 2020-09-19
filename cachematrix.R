## This code creates a pair of functions
## Together these functions create and cache the inverse of a matrix
## This speeds up the process of retrieving the inverse

## Section 1
## Create a makeCacheMatrix function to create a matrix object and
## then cache its inverse
## This function creates four functions to set and get
## the values of the matrix x and inverse s
## These functions are returned within a list to the parent environment

## Start by initializing the objects x and s
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
  
    ##Set the value of the matrix
    set <- function(y){
        x <<- y  ##This assigns the value of y to x in the parent environment
        s <<- NULL  ##This assigns s as null in the parent environment
                    ## This clears any prior cached value of s
    }
  
    ##Get the value of the matrix
    get <- function()x  ##Retrieves the matrix x from the parent environment
  
    ##Set the value of the inverse
    ##This assigns the value of the inverse of matrix s to setsolve 
    ##using value of s from parent environment
    setsolve <- function(solve) s<<- solve
  
    ##Get the value of the inverse
    getsolve <- function() s
  
    ##Assign each function to list
    ##Name each element so that $ operator can be uesd in cacheSolve
    list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}

## Section 2
## Create cacheSolve function to retrieve inverse from makeCacheMatrix()
## cacheSolve computes the inverse of matrix returned by makeCacheMatrix
## If inverse has already been calculated and x has the same value
## then cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {

    ##Retrieve inverse of matrix x
    s <- x$getsolve()
  
    ##Check whether the result is null
    ##If the value is not null, there is a cached value of s to retrieve
    if(!is.null(s)){
        message("Getting cached data...")
        return(s)
    }
  
    ##If s is null, then nothing will be returned above
    ##Then, we will use the get function to retrieve the input matrix
    data <-x$get()
    ##and use the solve function to calculate the inverse of the matrix x
    s <-solve(data,...)
    ##use setsolve() on the input to set the inverse
    x$setsolve(s)
    ##returns value of the inverse to the parent environment by printing
    s
}
