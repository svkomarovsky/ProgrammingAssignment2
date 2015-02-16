## Caching the inverse of a Matrix. The first function caches an inverse of a matrix. Second function first check
## if an inverse was already calculated and if yes, returns pre-computed value. Otherwise it calculates the inverse
## and call the first function to cache it.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # variable to store cached inverse matrix
  i <- NULL
  set <- function(y) {
    #initialze new matrix
    x <<- y
    #reset value of inverse matrix, will recalculate for new matrix
    i <<- NULL
  }
  get <- function() x
  setinverse <- function (inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getinverse()

   if (!is.null(m)) {
     message("getting cached inverse matrix")
     return (m)
   }
   data <- x$get()
   i <- solve(data,...)
   x$setinverse(i)
   i
   
}

## To test this functions:
##  1. Create numeric square matrix e.g. x <- matrix(c(2,7,3,1),nrow =2,ncol=2)
## 2. Create special "cachable" matrix m <- makeCacheMatrix(x)
## 3. Get inverse of the matrix m$getinverse()
## 4. Repeat step 3. Confirm message "Getting cached inversed matrix" appear. Matrix should also have same values 
## as in step 3.
