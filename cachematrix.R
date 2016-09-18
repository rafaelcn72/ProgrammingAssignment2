## R Programming - Roger Peng / Coursera
## Programming Assignment 2 - Lexical Scoping
## Week 3 - Assignment: Caching the Inverse of a Matrix

## COMMENTS:
## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly (there are also alternatives to matrix inversion that
## we will not discuss here). Your assignment is to write a pair of
## functions that cache the inverse of a matrix.

## WRITE THE FOLLOWING FUNCTIONS:

## 1. makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  m_inv <- NULL

## Set and Get functions
  
  set <- function(y) {
    
    x <<- y
    m_inv <<- NULL
  }

get <- function() x

setmatrix <- function(matrix) m_inv <<- matrix

getmatrix <- function() m_inv

list(set = set, get = get,
     setmatrix = setmatrix,
     getmatrix = getmatrix)
  }


## 2. cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## This function will return a matrix that is the inverse of 'x'
  
  m_inv <- x$getmatrix()
  
    if (!is.null(m_inv)) {
    
    message("getting cached data")
    
    return(m_inv)

    }
  
    inv <- x$get()
    
    m_inv <- solve(inv, ...)
  
    x$setmatrix(m_inv)
    
    m_inv
  }