## The following code provides a compact way to avoid unnecessary computations 
## of inverse matrices. 
## Example of use: 
## x <- matrix(c(2,0,0,2), nrow =2, ncol= 2)
## c <- makeCacheMatrix(x) # 'c' is the object that will cache the inverse of 'x' 
## inv <- cacheSolve(c)  # this computes the inverse of 'x' and caches it in 'c'
## new.inv <- cacheSolve(c) # now this just retrieves the value already computed


## 'makeCacheMatrix' returns a list that provides access to 4 functions: 'set', 
## 'get', 'setinverse', 'getinverse'. The variable 'inverse' caches the inverse 
## of 'x' (if it has already been computed).
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function(){
        x  
    } 
    setinverse <- function(inv){
        inverse <<- inv  
    } 
    getinverse <- function(){
        inverse  
    } 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)     
}


## 'cacheSolve' checks whether the inverse of 'x' has already been computed. 
## If it is the case it returns the already computed value and if it is not it 
## calls 'solve' to compute the inverse.
## 'x' is supposed to be a list of the type returned by 'makeCacheMatrix'. In 
## particular it must contain function members named 'get', 'getinverse' and 
## 'setinverse'.  
cacheSolve <- function(x, ...) {    
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse    
}
