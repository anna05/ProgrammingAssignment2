## The following two functions can be used in conjunction to cache the inverse of a matrix and then retrieve the 
## cached inverse if the matrix has not changed.
##
##
## The first function makeCacheMatrix has one argument 'x' which is a matrix. The function creates a special 
## "matrix" object, which is really a list containing a function to:
## 1.  set the value of the vector
## 2.  get the value of the vector
## 3.  set the value of the mean
## 4.  get the value of the mean
##
##
## The second function cacheSolve requires the output from the function makeCacheMatrix as its argument. It 
## calculates the inverse of the special "matrix" object created with makeCacheMatrix. But, it first checks to see 
## if the inverse has already been calculated. If it has is gets the inverse matrix from the cache (parent 
## environment) and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value 
## of the inverse in the cache (parent environment) via the `setmean` function.



## This function creates a special "matrix" object that can cache its inverse (store the inverse in a parent 
## environment).

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  #creating an empty variable to store the inverse matrix
    
    # Function which stores the input matrix 'y' into 'x' in the parent environment and resets the variable that 
    # stores the inverse matrix in the parent environment to empty
    set <- function(y) {   
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x                              #function which gets the matrix stored in the parent environment 
    setinverse <- function(inverse) inv <<- inverse  #function which stores the inverse matrix in the variable 'inv' in the parent environment
    getinverse <- function() inv                     #function which gets the cached inverse matrix stored in the parent environment
    
    #outputs a list with each element of the list representing one of the four functions defined above
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()  #getting the inverse matrix stored in the parent environment and storing in 'inv' in the global environment
    
    if(!is.null(inv)) {                 #checking to see if there is an inverse matrix stored in the parent environment
        message("getting cached data")  #message informing user that cached inverse matrix is being used
        return(inv)                     #returns the cached inverse matrix that was stored in the parent environment
    }
    
    #if the parent environment doesn't have an inverse matrix stored it needs to calculate the inverse matrix:
    
    data <- x$get()         #getting the matrix stored in the parent environment in order to calculate the inverse matrix
    inv <- solve(data,...)  #calculating the inverse matrix
    x$setinverse(inv)       #storing the inverse matrix in the parent environment
    inv                     #outputting the inverse matrix
}

}  