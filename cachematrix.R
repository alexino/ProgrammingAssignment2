#Here there are 2 functions that calculate the inverse of a matrix (using cache when available)


## 1.makeCacheMatrix() 
makeCacheMatrix <- function(x = matrix()) {
  ## it creates a special "matrix" object
  ## which is really a list containing a function to
  ##  set and get  the value of the matrix
  ##  set and get the value of it`s inverse
  

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


##2. cacheSolve() 
cacheSolve <- function(x, ...) {
  ##it returns a matrix that is the inverse of 'x'
  ##If the inverse has already been calculated it retrieve the inverse from the cache      

      
  i <- x$getinverse()
  if (!is.null(i))
  {
    #getting cached data
    return(i)
  }
  data <- x$get() #get original matrix
  i <- solve(data) #calculate the inverse matrix
  x$setinverse(i) #store results in the cachematrix object 
  
  return(i)
      
      
      
}
