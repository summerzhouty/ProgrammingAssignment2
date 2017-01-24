## Here I write 2 funtions which can be used to cache data of inverse matrix. 
## If we want to calculate an inverse matrix which has been already calculated before, instead of re-calculate it again, 
## we can just store it in a cache first, and directly recall it from the cache without time-consuming re-calculation.

## makeCacheMatrix function builds 4 basic functions and returns the functions within a list to the parent environment

makeCacheMatrix <- function(x = matrix()) { ## assign default value to argument x, which is an empty matrix
    IM <- NULL ## initialize IM as an object, which need to be used later
    set <- function(y){   ## set the value of the matrix and clear IM in memory
        x <<- y
        IM <<- NULL
    }
    get <- function() x  ## get the value of the matrix
    setIM <- function(y) IM <<- y  ## set the value of inverse matrix
    getIM <- function() IM ## get the value of inverse matrix
    list(set=set, get=get, setIM=setIM, getIM=getIM) ## return to a list which contains four functions
}


## cacheSolve function first checks whether the value of inverse matrix has already been stored in cache.
## If yes, this function would return the value stored in cache;
## If no, this function would calculate it, and store it in cache so that we can directly use it next time.

cacheSolve <- function(x, ...) {
    IM <- x$getIM()  ## get value stored in getIM()
    if(!is.null(IM)){  ## check whether the value is NULL or not
        message("getting cached data")
        return(IM)  ## if exist, just break the cacheSolve function and return the value stored in cache
    }
    data <- x$get() ## get data by calling function get()
    IM <- solve(data)  ## assign the value of inverse matrix to IM
    x$setIM(IM) ## set IM to the cache
    IM ## return the value of IM
}
