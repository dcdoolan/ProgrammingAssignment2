## Caching the Inverse of a Matrix
## These functions are used to calculate the inverse of a matrix, saving it to a cache
## therefore at the next execution of the Inverse Matrix the cached value will be returned
## without the need to rerun the computation

## This function creates a special "matrix" object, which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ## establish the cache
    set <- function(y) {  ##matrix setter function
        x <<- y 
        m <<- NULL 
    }
    get <- function() x   ##matrix getter function
    setinverse <- function(inverse) m <<- inverse  ##setter for inverse function 
    getinverse <- function() m                     ##getter for inverse function
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ##build the list
}


## This function is used to inverse a matrix, firstly checking if the
## inverted matrix has be already created (cached). If its cached then the
## cached result is returned, if not then the inverse is calculated, 

cacheSolve <- function(x, ...) { ## Return the matrix (inverse of x)
    m <- x$getinverse()
    if(!is.null(m)) { ## if it already exsits return the cached data
        message("getting cached data")
        return(m)
    }
    data <- x$get()  ## else compute and return the inverse matrix
    m <- solve(data)
    x$setinverse(m)
    m
}

