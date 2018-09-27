## Put comments here that give an overall description of what your
## functions do

##Combined these functions are designed to initially take an invertible matrix in the function
##makeCachematrix and turn it and the functions we wish to be done to it and turn them into a list.
##This list is then used as an argument in cacheSolve, which in return with solve for the inverse of
##the matrix we used as an argument in the first function. However, if the inverse has been already found
##that same function will have saved it in the cache so that it will not need to be computed again.

## Write a short comment describing this function

##This is the function that turns the matrix we want the inverse of, the variable it will eventually
##be saved to and returned, and the function Solve all into a list so that it can later be passed in
##the cacheSolve function.

makeCacheMatrix <- function(x = matrix()){
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Write a short comment describing this function
##This function accepts the list that was created in the previous function. The function then begins
##by checking to see if the inverse has already been found by checking the getInverse part of the list.
##This part will return m itself. If m is still set to NULL from the previous function, cacheSolve will
##continue forward. It then uses x$get to retrieve the original matrix we passed and assigns it to a
##new variable so that it can be solved for the inverse and then stored to m in the current scope.
##The tricky part comes from the next line x$setInverse(m) which uses the <<- to save the new inverse
##matrix to m in the environment of the first function so that it can later be used without having to
##solve it begin because m will no longer be NULL when cacheSolve is called.

cacheSolve <- function(x, ...){
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
        ## Return a matrix that is the inverse of 'x'
