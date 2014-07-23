## ===================================================================
##
## Matrix inversion is usually a costly computation and their may be 
##      some benefit to caching the inverse of a matrix rather than 
##      compute it repeatedly. The function "makeCacheMatrix" caches 
##      the inverse of a matrix and "cacheSolve" retrieves the inverse 
##      from the cache (if the inverse has already been calculated) or 
##      computes it.
##      
## ===================================================================
## 
## The "makeCacheMatrix" function  creates a special "matrix" object
##      that can cache its inverse
##

makeCacheMatrix <- function(x = matrix()) {
        ## Inicialize the inverse matrix
        m <- NULL
        ## Set and Get the value of the matrix
        set <- function(y){
                        x <<- y
                        m <<- NULL
        }
        get <- function() x
        ##  Calculate the inverse of the matrix by solve function and cache it
        setmatrixInv <- function(solve) m <<- solve
        getmatrixInv <- function() m
        ##  Get the return value of the function "makeCacheMatrix"
        list(set = set, 
            get = get,
            setmatrixInv = setmatrixInv,
            getmatrixInv = getmatrixInv)
}
## ================================================================
## 
## The "cacheSolve" function computes the inverse of the special 
##      "matrix" returned by function "makeCacheMatrix". If the 
##      inverse has already been calculated and it 
##      has not changed, then the  "cachesolve" function should 
##      retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrixInv()
        ## Verify if the matrix has cached and retreive, case positive
        if(!is.null(m)){
            message("getting cached data")
            return(m)
        }
        ##  Calculate the inverse matrix, if has not cached
        matrix <- x$get()
        m <- solve(matrix, ...)
        ## Cache the matrix
        x$setmatrix(m)
        m
}

## ================================================================ 
## 
## EXAMPLE OF USE
##
## Creating the matrix
## 
## > u<-rbind(c(1, -1/4), c(-1/4, 1))
## > u
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
## 
## Creating a special "matrix" object that can cache its inverse
## 
## > v<-makeCacheMatrix(u)
## 
## Getting the inverse matrix
## 
## > cacheSolve(v)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## 
## Verifying if the function retrieved the inverse from the cache
##
## > cacheSolve(v)
## getting cached data
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## 
## Verifying that you get the inverse matrix
## 
## > cacheSolve(v)%*%u   
## getting cached data
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## >
##
