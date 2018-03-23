makeVector <- function(x = numeric()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmean <- function(mean) m <<- mean
      getmean <- function() m
      list(set = set, get = get,
           setmean = setmean,
           getmean = getmean)
}


cachemean <- function(x, ...) {
      m <- x$getmean()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- mean(data, ...)
      x$setmean(m)
      m
}

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y){
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      getInverse <- function() inverse
      setInverse <- function(solveMatrix) inv <<- solveMatrix
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
      inverse <- x$getInverse()
      if(!is.null(inverse)){
            message
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data)
      x$setInverse(inverse)
      inverse      
}