## First function Creates an inverted matrix and caches it
## Second function checks for a cached inverted matrix and 
##returns it if it exists or creates one and prints it



## Creates an inverted matrix and caches it

makeCacheMatrix <- function(x = matrix()) {  #for created a cached matrix
  m_inv <- NULL  # sets value of m_inv to NULL (default value in case cacheSolve is yet to be used)
  set <- function(y) {  #sets matrix value
    x <<- y  #caches inputted matrix for cacheSolve to check if values have changed
    m_inv <<- NULL  #sets value of m_inv (inversed matrix if used cacheSolve) to NULL
  }
  get <- function() x #gets data
  setMat_Inv <- function(solve) m_inv <<- solve #caches inverted matrix
  getMat_Inv <- function() m_inv #solves for inverted matrix 
  list(set = set, get = get, # creates a list to store functions
       setMat_Inv = setMat_Inv,
       getMat_Inv = getMat_Inv)
}


## Returns an inverted matrix, after checking if it has been cached earlier


cacheSolve <- function(x, ...) { #for checking if a previous value has been cached
  m_inv <- x$getMat_Inv()  #value to be used if an earlier Matrix has been inverted and input values have not changed
  if(!is.null(m_inv)) { #checks if cacheSolve has run earlier and stores data
    message("getting cached data")
    return(m_inv) #provide inverted matrix
  }
  data <- x$get() #gets new values, if changed
  m_inv <- solve(data, ...) #inverts matrix
  x$setMat_Inv(m_inv) 
  m_inv #prints inverted matrix
}
