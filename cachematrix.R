## Collection of functions to store matrices and compute their inverse
## When computed, the inverse is also store for reuse and is not recalculated
## for a given data set


## Creates a list that contains the functions to store and retrive the a
## matrix and its inverese
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(m_inv) inv <<- m_inv
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns the inverse of a matrix created by 'makeCaheMatrix'
## The result is either computed or retrived from cache
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
