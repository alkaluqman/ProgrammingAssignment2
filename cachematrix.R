## Return cached matrix inverse

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(mat){
                x <<- mat
                inverse <<- NULL       
        } 
        get <- function() x
        setInverse <- function(inv){
                inverse <<- inv 
        }
        getInverse <- function() inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function returns the inverse from cache if inverse is present in cache,
## else solves the inverse and adds to cache for further use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache_check <- x$getInverse()
        if(!is.null(cache_check)){
                message("Retrieving inverse from cache")
                return(cache_check)
        }
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}
