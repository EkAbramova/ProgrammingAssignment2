
## This function creates a special "matrix" object that can cache its inverse

## set the value of the matrix
## get the value of the matrix
## set the value of the of inverse of the matrix
## get the value of the of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve_m) m <<- solve_m
        getsolve <- function() m
        invisible(list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve))
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setsolve(m)
        m
}

## Testing data
## > m_a <- rbind(c(3, 4), c(2, 5))
## > f_m <- makeCacheMatrix(m_a)
## > cacheSolve(f_m)
## [,1]       [,2]
## [1,]  0.7142857 -0.5714286
## [2,] -0.2857143  0.4285714

## The second run - get cached data
## > cacheSolve(f_m)
## getting cached data
## [,1]       [,2]
## [1,]  0.7142857 -0.5714286
## [2,] -0.2857143  0.4285714