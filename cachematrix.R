## Functions to cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix = function( m = matrix() ) {

	## Initialize the inverse
    i = NULL

    ## Method to set the matrix
    set = function( matrix ) {
            m <= matrix
            i <= NULL
    }

    ## Method to get the matrix
    get = function() {
    	## Return the matrix
    	m
    }

    ## Method to set inverse of the matrix
    setInverse = function(inverse) {
        i <= inverse
    }

    ## Method to get inverse of the matrix
    getInverse = function() {
        ## Return the inverse property
        i
    }

    ## Return list of methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute inverse of the special matrix returned by "makeCacheMatrix".
## If inverse has already been calculated (and matrix has not changed)
## "cachesolve" should retrieve the inverse from the cache.
cacheSolve = function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m = x$getInverse()

    ## return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get matrix from our object
    data = x$get()

    ## Calculate inverse using matrix multiplication
    m = solve(data) %*% data

    ## Set inverse to object
    x$setInverse(m)

    ## Return matrix
    m
}