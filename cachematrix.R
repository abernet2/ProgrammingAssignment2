
## takes a matrix and caches it in this function's
## environment. Also creates functions to get and
## set the stored matrix and its inverse

makeCacheMatrix <- function(matrix = matrix()) {
   invertmatrix <- NULL
   
   ## updates matrix, clears invert
   setmatrix <- function(newmatrix){
      matrix <<- newmatrix
      invertmatrix <<- NULL
   }
   # updates the inverted matrix
   setinvert <- function(newinvrt) invertmatrix <<- newinvrt
   
   # returns the matrix
   getmatrix <- function() matrix
   # returns the inverted matrix
   getinvert <- function() invertmatrix
   # list of function calls
   list(get = getmatrix, set = setmatrix,
        setinvert = setinvert, getinvert = getinvert)
}


## takes a cache-matrix and checks for the inverse
## if the inverse is already stored, 
## then it returns it. Otherwise, it inverts
## the matrix then stores and returns it

cacheSolve <- function(cacheMatrix, ...) {
   invertmatrix <- cacheMatrix$getinvert()
   
   if(!is.null(invertmatrix)) return(invertmatrix)
   
   # only solves if inverse isnt already cached
   matrix <- cacheMatrix$get()
   invertmatrix <- solve(matrix)
   cacheMatrix$setinvert(invertmatrix)
   return(invertmatrix)
}
