## makeCacheMatrix - a cache function that holds a source matrix, the inverse of that source matrix and
##    set/get accessor functions for both the source matrix, and its inverse
## cacheSolve - a function that returns the inverse of a source matrix. If that inverse has already been calculated
##    and cached, the cached value is returned -otherwise- the inverse is calculated and cached, before being returned

## makeCacheMatrix
## This function serves as an Inverse Matrix cache - storing a source Matrix and its associated inverse -
## while providing set/get accessor functions for both the source matrix and its associated inverse

makeCacheMatrix <- function(lMatrix = matrix()) {
  
  ## This function has 3 states: 
  ## (1) source matrix is not defined
  ##    length(get()) = 0
  ## (2) source matrix is defined, associated inverse is has not been calculated
  ##    length(get()) > 0 and is.null(getInverse())
  ## (3) source matrix has been defined, associated inverse has been calculated
  ##    length(get()) > 0 and !is.null(getInverse())
  
  ## arguments:
  ##    1. lMatrix - the source Matrix; default is an empty matrix; may also be defined  via set())
  
  ## returns:
  ## a list of set/get accessor functions
  ##    1. set( sourceMatrix )         - set the source matrix
  ##    2. get()                       - get the source matrix
  ##    3. setInverse( inverseMatrix ) - set the associated inverse
  ##    4. getInverse()                - get the associated inverse
  
  ## usage:
  ##    1. set the sourceMatrix (a) as an argument when this object is created, or (b) via the set() function
  ##    2. get the associated inverse via the getInverse() function
  ##    3. if the associated inverse is null, calculate the associated inverse, and define it via the setInverse()
  ##       function
  
  lInverse <- NULL
  
  ## set/get functions
  
  ## 1. set source matrix (inverse becomes undefined)
  ##    nMatrix = source matrix
  set <- function( nMatrix ) {
    lMatrix <<- nMatrix
    lInverse <<- NULL
  }
  
  ## 2. get source matrix
  get <- function() {return( lMatrix )}
  
  ## 3. set associated inverse
  ##    nInverse = associated inverse
  setInverse <- function( nInverse ) {lInverse <<- nInverse}
  
  ## 4. get associated inverse
  getInverse <- function() {return( lInverse )}
  
  ## return set/get accessor functions
  return( list( set=set,get=get,setInverse=setInverse,getInverse=getInverse ))
}


## cacheSolve
##    a function that returns the inverse of a source matrix. If that inverse has already been calculated
##    and cached, the cached value is returned -otherwise- the inverse is calculated and cached, before being returned

cacheSolve <- function(x, ...) {
  
  ## arguments:
  ##    1. x - A makeCacheMatrix object which contains a source Matrix, and may contain (cached) the inverse of that 
  ##           source. If the inverse has been cached, it may be used as is. If the inverse has not been cached, it
  ##           must be calculated and cached before being returned.
  ##           x also contains set/get accessor functions for both the source matrix, and its inverse
  ##    2. ... - additional arguments for solve()
  
  ## returns:
  ##    The invrese of the source matrix defined by x
  
  ## get the inverse matrix. If it is not yet defined(=null), calculate and cache. If it is defined, it may be
  ## returned as is
  lInverse <- x$getInverse()
  
  if (is.null( lInverse )) {
    ## The inverse is not yet defined ... calculate & cache
    ## 1. get the source matrix
    lMatrix <- x$get()
    ## 2. calculate the inverse
    lInverse <- solve( a=lMatrix, ... )
    ## 3. cache the inverse
    x$setInverse( lInverse )
  }
  
  ## return the inverse
  return( lInverse )
}
