
## Put comments here that give an overall description of what your
## functions do
## This is the second programming assignment of the Programming in R Course
## It consists in a couple of functions to cache the inverse of a matrix

## Write a short comment describing this function
## The function makesCacheMatrix, creates a special "matrix", which is really a list
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## The function cacheSolve calculates the inverse of a special "matrix" created 
## with the above function. It first check to see if the inverse has already been calculated.
## If so, it gets the inverse matrix from the cache and skips the computation. Otherwise it calculates the inverse and sets the value using the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Sample of Execution
# Creates a square matrix and calculates it???s inverse
# 
# > M <- rbind(c(1, -1/4), c(-1/4, 1))
# > M
# [,1]  [,2]
# [1,]  1.00 -0.25
# [2,] -0.25  1.00
# > solve(M)
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
# 
# Makes the special Matrix Object
# 
# m <- makeCacheMatrix(M)
# > m$get()
# [,1]  [,2]
# [1,]  1.00 -0.25
# [2,] -0.25  1.00
# 
# Retrieves first time with no cache
# 
# 
# Retrieves second time from cache
# 
# > cacheSolve(m)
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
# 
# > cacheSolve(m)
# getting cached data.
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667 
