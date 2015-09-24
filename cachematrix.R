
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
## Returns the inverted matrix if already stored
## Otherwise it inverts the matrix then stores/returns

cacheSolve <- function(cachematrix, ...) {
   invertmatrix <- cachematrix$getinvert()
   
   if(!is.null(invertmatrix)) return(invertmatrix)
   
   # only solves if inverse isnt already cached
   matrix <- cachematrix$get()
   invertmatrix <- solve(matrix)
   cachematrix$setinvert(invertmatrix)
   return(invertmatrix)
}
