## Here I write 2 funtions which can be used to cache data of inverse matrix. 
## If we want to calculate an inverse matrix which has been already calculated before, instead of re-calculate it again, 
## we can just store it in a cache first, and directly recall it from the cache without time-consuming re-calculation.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    IM <- NULL
    set <- function(y){
        x <<- y
        IM <<- NULL
    }
    get <- function() x
    setIM <- function(y) IM <<- y
    getIM <- function() IM
    list(set=set, get=get, setIM=setIM, getIM=getIM)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    IM <- x$getIM()
    if(!is.null(IM)){
        message("getting cached data")
        return(IM)
    }
    data <- x$get()
    IM <- solve(data)
    x$setIM(IM)
    IM
}
