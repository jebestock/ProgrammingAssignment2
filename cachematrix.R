## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m    <- NULL
    minv <- NULL
    setMatrix    <- function(y){
        m    <<- y
    }
    setInvMatrix <- function(y){
        m_inv <<- y
    }
    getMatrix    <- function(){
        m
    }
    getInvMatrix <- function(){
        m_inv
    }
    list(setMatrix    = setMatrix,
         setInvMatrix = setInvMatrix,
         getMatrix    = getMatrix,
         getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function
##
## compute the inverse only if the cache is empty or
## the input matrix is unequal to the matrix from which
## the cached inverse was computed from.
## After a new matrix is computed memorize both
## the matrix and its inverse in the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m_inv <- x$getInvMatrix()
    m     <- x$getMatrix()
    if( !is.null(m_inv) && m==x){
        message("getting cached inverse matrix")
        return(m_inv)
    }
    new_m <- x$get()
    m_inv <- solve(new_m,...)
    x$setMatrix(new_m)
    x$setInvMatrix(m_inv)
    return(m_inv)
}
