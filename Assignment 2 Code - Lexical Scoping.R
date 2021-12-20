#The first function, makeVector creates a special "vector", which is really a list containing a function to
#1. set the value of the vector
#2. get the value of the vector
#3. set the value of the inverse
#4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#The following function calculates the inverse of the special "vector" created with the above function. 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

#Assignment
B <- matrix(c(1,2,3,4),2,2)

B1 <- makeCacheMatrix(B)
#This is the inverse returned after the computation
cacheSolve(B1)

#This is the inverse returned from cache
cacheSolve(B1)









