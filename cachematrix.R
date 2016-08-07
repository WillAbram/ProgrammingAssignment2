## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix() and cacheSolve() work together in exploiting the lexical scoping 
# capabilities of R to calculate the inverse of a matrix and return that calculate value, 
# in future calls, from a cache instead of a recalculation. The functions also provide for
# specifying a new matrix to which to do the same while bypassing a rebuild all of
# the necessary objects.

## Write a short comment describing this function

# makeCacheMatrix() defines a series of functions which work together to calculate and cache
# the inverse of a given (invertable) matrix onto their scope chain, which they share by 
# having been created within the same parent environment. It then builds these funtions 
# into a list to be accessed by a subsequent function which will execute the functions of 
# the list. It returns the list of functions, one of which, set(), can also be accessed in
# the workspace to change the matrix to be inversed and its inverse cached.

# get() stores the target matrix making it available for calculation in cacheSolve().

# setINV() - actually caches inverse in subsequent cacheSolve funtion by pushing calculated
# value up a level in the scope chain onto the shared parent environment. 

# getINV() - holds calculated value, initially set to NULL, later in subsequent calls is 
# placeholder or pointer to where the invere is cached.

makeCacheMatrix <- function(x = matrix()) {
      I <- NULL
      set <- function(y) {
            x <<- y
            I <<- NULL
      }
      get <- function() x
      setINV <- function(INV) I <<- INV
      getINV <- function() I
      list(set = set, get = get,
           setINV = setINV,
           getINV = getINV)
}

## Write a short comment describing this function

# cacheSolve() actually executes the functions built in makeCacheMatrix in a conditional 
# sequence to return the calculated inverse if it has been cached or, if it hasn't yet, 
# calculate the inverse and cache it so that it can be returned without recalculation on 
# subsequent calls. The variable "I" is assigned, via getINV(), from the parent environment 
# of the getINV() function, and is either NULL upon initiation or the cached, calculated 
# value of the invere of I. If I is NULL then the inverse of the matrix is calculated and
# then cached by setINV(). While the value of I in cacheSolve is garbage collected, the 
# updated value of I is retained in the scope chain of getINV() which will assign that
# updated value to I in cacheSolve() in the first line of cachesolve().

cacheSolve <- function(x, ...) {
      I <- x$getINV()
      if(!is.null(I)) {
            message("getting cached data")
            return(I)
      }
      data <- x$get()
      I <- solve(data, ...)
      x$setINV(I)
      I
}

# To test the funtions run these commands:

target_matrix <- cbind(c(0, 2, 1), c(0, -1, 1), c(1, 3, 4))
matrix_pack <- makeCacheMatrix(target_matrix)
cacheSolve(matrix_pack)
cacheSolve(matrix_pack)
