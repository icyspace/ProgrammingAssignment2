##This funciton accepts a matrix as x.  It contains 4 functions to 
## get and set the matrix and get and set the inverse.  It includes
## two data object x (mentioned above) and Y
## The Funciton allows for the inverse of the metrix to be cached after 
## it hase been calculated.  It will check for the cached inverse prior
## to calculating it.  If avalible it will return from cache and not re-
## calculate. 


##The first function, makeCacheMatrix creates a special "metrix", 
##which is really a list containing a function to
##
## 1 set the value of the metrix
## 2 get the value of the metrix
## 3 set the value of the inverse of the metrix
## 4 get the value of the inverse of the metrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  # set does two things sets x in the parent to Y passed in to the set 
  # function and sets m to null (clean prior cached inversed matrixs)
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get return x as the metrix
  get <- function() x
  # sets the inverse of the function(used by cachSolve())
  setinverse <- function(inv) m <<- inv
  #gets the cached inverse 
  getinverse<- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  #checks m if not null then inverse has  been calculated for x
  #and returns M. Otherwise continues to calculate inverse of x and sets
  #it to m.  Using the setinverse funciont to cache the inverse
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
        ## Return a matrix that is the inverse of 'x'
}
