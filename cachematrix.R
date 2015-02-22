## i think makeCacheMatrix operates as a container/methods
## for the overall caching; it doesn't seem to do any of the 
## heavy lifting, rather it just describes rules for assigning
## values for the matrix, the cache value, and the solution
## (inverse). Does not compute the inverse.

## the actual computationally useful thing is done by cacheSolve, 
## which uses the container tools in makeCacheMatrix to 
## see if it needs to bring in the "big guns" to solve
## this matrix

## makeCacheMatrix creates an empty container which has a method
## for filling it called $set. once it has been filled, the value
## of the inverse (inv) is set to NULL, and other
## methods can look at the data stored in x by using $get
## and do computations, the results of which they save in 
## inv with $setinverse.
## once this has been done, the value of the inverse (stored in 
## m and accessed by $getinverse) switches from NULL to whatever
## the outer function deems best. subsequent calls (without another
## $set command, which would wipe it out) just look up this stored
## inv value.

makeCacheMatrix <- function(x = matrix()) {
    
    inv = NULL
    set <- function(y) {
        x <<- y # so x takes on a life defined by y, outside of this func
        inv <<- NULL # inv is living large and getting a fresh start since x got clobbered
        # is '<<-' something like a "return" type statement?
    }
    get <- function() x # boring function that returns whatever x is now
    setinverse <- function(inverse) inv <<- inverse #ok what's the difference here from just having m returned?
    getinverse <- function() inv
    
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}


## solves for the inverse; first looks to see if there's
## already something in the inverse.
## what i don't like about this is I could theoretically
## use some other command to set inv<- "Party on, dude!"
## and that would stop cacheSolve from actually doing
## anything useful with this matrix. Not that there's
## anything wrong with that :)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}


## testing it out

mat <- matrix(runif(9)*20,nrow=3,ncol=3)
foo <- makeCacheMatrix()
foo$set(mat)
cacheSolve(foo)
tmp <- cacheSolve(foo)
round(tmp %*% mat,3)
# rinse, repeat
    
mat2 <- matrix(runif(9)*20,nrow=3,ncol=3)
foo$set(mat2)
cacheSolve(foo)
tmp <- cacheSolve(foo)
round(tmp %*% mat2,3)
    
foo$setinverse('Party on, dude!')
cacheSolve(foo)
