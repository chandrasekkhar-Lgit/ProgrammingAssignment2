##Below functions are written to eiliminate repetitive calculations of matrix inversions
##when input matix data is variable which might have similar elemenst as in past.
##Same would be obtained by creating two functions- makeCacheMatrix and CacheSolve.

##makeCacheMatrix() creates a matrix in form of R object and stores the matrix 
##and the inverse of matix.Returns list of functions as set(), get(),setinv() 
##and getinv().

makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL    # inv represents Inverse of Matrix.Here assigning NULL value.
    
    # set function inputs argument to x in the Parent environment and assigns
    #NULL value to the inv object.
    set<-function(y){ 
          x<<-y
          inv<<-NULL
    }   
    
    get<-function() x   # X is called as argument from parent environment.  
    
    set_inv<-function(solvex) inv<<-solvex #set_inv function is created.    
             #input argument is assigned to inv in parent environment.
    
    get_inv<-function() inv  # getter function for the elements of inv. 
    
    list(set=set, get=get, set_inv=set_inv, get_inv=get_inv) 
    # returns makeCacheMatrix() object, with above mentioned functions
    #as the elements of list.
}


## cacheSolve function calls the argument that is returned from makeCacheMatrix().

cacheSolve <- function(z, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## cacheSolve() would be populating the inverse of matrix from object of makeCacheMatrix()
    
    inv<-z$get_inv() # calls get_inv function on the input object
  
    # For a new matrix since inv is set to NULL in makeCacheMatrix(), 
    #if the argument is not NULL, we have valid Cached inv values and would return it.  
        if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    } 
    
    #if !is.null() is false,then below is execution of Inverse of Matrix function   
    #by fetching data from input matrix, calculates inverse of Matrix. Uses set_inv
    #to set the inverse of matrix in the input object i.e.makeCacheMatrix. Then returns inv value
    
    data<-z$get()
    inv<- solve(data,...)
    z$set_inv(inv)
    inv
}
