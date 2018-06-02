## In this passage there are two functions. The first function takes as argument a matrix "X" and 
## return a list of function. In this function is computed the inverse matrix of "X". In the second 
## function, it is called the first function to recover the value of the inverted matrix.

## The function return 4 other function. First (set) to set the matrix. Second (get) to get the matrix.
## Third (setinverse) to computed the inverse matrix. Fourth (getinverse) to store the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set <- function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setinverse<-function(solve) m<<-solve
    getinverse<-function() m
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function computed the inverted matrix starting from the special "matrix" of the previous function.
## In case it doesn't find a matrix, it proceeds to make the calculation, otherwise it stores the matrix evaluated 
## in the previous function

cacheSolve <- function(x, ...) {
    
    m<-x$getinverse()
    if (!is.null(m)){
        message("getting cached data") 
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setinverse(m)
    m
    ## Return a matrix that is the inverse of 'x'
}
