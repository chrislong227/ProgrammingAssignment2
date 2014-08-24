# makeCacheMatrix:  This function creates a special "matrix" object that
#                   can cache its inverse

makeCacheMatrix <- function(x = matrix()) { # The input will be a matrix
  
  m <- NULL   # m is the inverse matrix and is reset to NULL every time 
              # the makeCacheMatrix is called
  
  set <- function(y) {  # Inputs a matrix
    
    x <<- y             # Saves the matrix
    m <<- NULL          # Reset the inverse to NULL
    
  }
  
  get <- function() x   # Returns the original matrix
  
  setmatrix <- function(solve)  # This is called by the first cacheSolve
    m <<- solve                 # access and will store the value  
  
  getinverse <- function() m    # This will return the value of cacheSolve
                                # on subsequent accesses
  
  list(set = set, get = get,    # This returns the created object
    setmatrix = setmatrix,
    getinverse = getinverse)
  
}


# cacheSolve: This function computes the inverse of the special "matrix"
#             returned by makeCacheMatrix above. 
#             If the inverse has already been calculated (and the matrix
#             has not changed), then the cachesolve should retrieve the 
#             inverse from the cache

cacheSolve <- function(x, ...) {  # The input is an object created by
                                  # makeCacheMatrix
  
  m <- x$getinverse()   # Access to x and solve the matrix
  
  if(!is.null(m)) {   # If the inverse is already calculated (not Null)
    
    message("getting cached data")  # Show this message on the Console
    return(m)                       # and shows the solution. End the 
                                    # cacheSolve calculation
  }
  
  data <- x$get()                   # If the inverse is not calculated
  
  m <- solve(data, ...)             # Get the inverse matrix of x
  
  x$setmatrix(m)                    # Cache the inverse matrix
  
  m                                 # Return the calculated inverse on
                                    # Console
}
