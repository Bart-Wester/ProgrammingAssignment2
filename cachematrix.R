## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
#set the value of the matrix using setm
#get the value of the matrix using getm
#set the value of the inverse matrix using setinv
#get the value of the inverse matrix using getinv

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    #set the value of the matrix
      setm <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    #get the value of the matrix
    getm <- function() x
    
    #set the value of the inverse matrix
    setinv <- function(solve) m <<- solve
    
    #get the value of the inverse matrix  
    getinv <- function() m
    list(setm = setm, getm = getm, setinv = setinv, getinv = getinv)
  }
  
## Return a matrix that is the inverse of 'x'
## get the value from x (listitem 'getinv') and assign to m
## check if any value has been assigned (has a matrix been calculated?)
## if so, exit the function, 
## returning the calculated inverse matrix from 'getinv'
## only if not, get the matrix from x (listitem 'getm')
## calculate the inverse matrix with solve,
## set it in x 
## return the newly calculated inverse matrix

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("inverse calculated, getting from cache")
    #and exiting
    return(m)
  }
  data <- x$getm()
  m <- solve(data, ...)
  #calculate inverse and set in cache
  x$setinv(m)
  m
}
