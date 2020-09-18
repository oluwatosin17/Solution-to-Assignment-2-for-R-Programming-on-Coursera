## Put comments here that give an overall description of what your
## functions do



makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(){
                     x <<- y
                     inv <<-NULL
}
            get <- function(){x}
            setinverse <- function(inverse){ inv <<- inverse}
            getinverse <- function() {inv}
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)

}




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- m <- x$getinverse()
      if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setinverse(inv)
      inv
}


zmatrx <- makeCacheMatrix(matrix(1:4, 2,2))
zmatrx$get()

cacheSolve(zmatrx)
