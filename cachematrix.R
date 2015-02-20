## This file contains 3 functions, which permits to source a single file for execution
## 1/ makeCacheMatrix is a function which purpose is to define functions that will be 
## called by the cacheSolve function. 
## 2/ cacheSolve is the function returning the actual reverse matrix, either by 
## computing it,or by geting it from the cache which is in the makeCacheMatrix environment
## 3/ an additional testmatrix function supplied to check that everything works as
## it should. In order to test you can enter:
        ## > source ('cachematrix.r')
        ## > testmatrix(10)

## Nota: the code is surely too much described, but it is for an academic purpose :-)



## makeCacheMatrix is a function that produces a list of 4 functions 
## set, get, setinv, getinv when called with x passed as a formal argument
## x being a matrix
## get, setinv, and getinv will be called by the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## inv is a variale which will at least contain the reverse matrix
        ## inv can be set up by calling the setinv function, which will super-assign
        ## a matrix to inv at this level (the environment of makeCacheMatrix)
        set <- function(y) {
                x <<- y         
                ## x is not here a local variable but y is super-assigned to the x 
                ## defined in the environment of the calling function makeCacheMatrix
                ## ie the formal arg x of makeCacheMatrix
                inv <<- NULL      
                ## same as for x, "the" inv defined in the body of the makeVector function
        }
        get <- function() x
        ## x is not an argument of the get function but a free variable
        ## the purpose of the get function is to return the x of makeCacheMatrix
        ## no brackets here as there is a single instruction
        setinv <- function(y) inv <<- y
        ## y is a formal arg of setinv, which is super-assigned to the inv
        ## defined in the body of the makeCacheMatrix function
        getinv <- function() inv
        ## getinv returns the inv defined in the body of the makeCacheMatrix function
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        ## prints out the content of the list of 4 functions
        
}

## cacheSolve returns a matrix that is the inverse of 'x'. It first checks that the
## reverse computation hasn't been allready carried out, by inspecting the cache of
## makeCacheMatrix. If the cache is "empty", the computation is performed and the 
## cache is updated. Otherwise, the cache is just being returned without computation

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        ## calls the getinv function which returns the inv defined in makeMatrix body
        ## note that inv has only a local significance (local variable) here
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
                ## if inv is not NULL, this means that setminv has allready been called 
                ## once (in cacheSolve). In that case, the inv returned here inv is the one
                ## defined in the body of the makeCacheMatrix function
        }
        data <- x$get()
        ## returns the x matrix, argument used while calling makeCacheMatrix
        inv <- solve(data, ...)
        x$setinv(inv)
        ## super-assigns the inverse of the data matrix to the inv of makeCacheMatrix
        inv
        ## note that this inv matrix diplayed is local to cacheSolve
}

## testmatrix function can be called with an argument n, being the size of a square matrix
## this matrix is being filled with random normal values and passed as a parameter
## to the makeCacheMatrix function, and then reversed by calling the cacheSolve function
##
## The first time cacheSolve ins called, the computation is performed
## Then cacheSolve is being called another time for the multiplication of the initial 
## matrix by its reverse matrix. We check that this time the cache value is being
## used without recomputing the reverse matrix, and that the result of the multiplication 
## is the Identity matrix, for which we round the first 10 lines, 10 collumns for display

## note that this is a very simple function which doesn't check n boundaries

testmatrix <-function(n){
        size<- n
        mymatrix <- matrix(rnorm(size^2), nrow=size, ncol=size)
        z<-makeCacheMatrix(mymatrix)
        cacheSolve(z)
        print('first time')
        
        id<-z$get() %*% cacheSolve(z)
        idround <-round(id[1:10,1:10])
        idround
}