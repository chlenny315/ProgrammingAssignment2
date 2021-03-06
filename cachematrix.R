# Bascailly we are making a function factory which contains a list
# of 4 functions: 
# 1) set - set the value of the matrix 
# 2) get - return of the input value of the matrix
# 3) setmatrix - set the value of inversed matrix
# 4) getmatrix - get the value of inversed matrix


makeCacheMatrix <- function(x = matrix()) {
      m <- NULL # placeholder for futures value of inversed matrix
      set <- function(y) {
            x <<- y 
            m <<- NULL # clear cached memory
      }
      get <- function()  x 
      setmatrix <- function(inverse)  m  <<- inverse # 
      getmatrix <- function()  m 
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix) 
}


# Return a value that is the inverse of object x
# It first checks to see if the inverse matrix has already been calculated. 
# If so, it gets the inverse matrix from the cache 
# and skips the computation. Otherwise, it calculates the inverse of the given 
# matrix and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...){
      m <- x$getmatrix()   
      if (!is.null(m)){    
            message ("getting cached data")
            return(m)      # if getmatrix is not NULL, return it           
      }
      matrix <- x$get() 
      m <- solve(matrix, ...)
      x$setmatrix(m)
      m
      
}

