## The first function, makecaheMatrix  creates a special "matrix",
#which is really a list containing a function to

#set the value of the matrix
#get the value of the matrix
#set the inverse of a square matrix
#get the inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
        nc<-nrow(x)
        m<-matrix(nrow=nc,ncol=nc)
        m <- numeric()
        set <- function(y) {
                x <<- y
                m<-matrix(nrow=nc,ncol=nc)
                m<-numeric()
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve(m)
        getinverse <- function() m
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
    
}


#The following function calculates the inverse of a matrix created with the above function
# However, it first checks to see if the inverse has already been calculated.
#If so, it gets theinverse from the cache and skips the computation
# Otherwise, it calculates the inverse  



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getinverse()
         if (!is.null(m[[ ]])){
                message("getting cached data")
                return(m)
         }
         data <- x$get()
         m <- solve(data, ...)
         x$setinverse(m)
         m
         
}
