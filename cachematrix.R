##  Programming assignment 2, R Programming, session rprog-012
##  Author: Rick Guggemos   Date: 3/20/15
##
##  To save processing time, we want to cache the inverse of a matrix
##
##  In order to do this, we are creating a list of functions titled: set, get, setinv, getinv.
##  These set or get the matrix to/from cache, and set or get the inversion to/from cache
##  
##  arg -   is a local matrix to hold the functions (or pointers to the func's) we create 
##  mtx -   is the matrix passed into set
##  cmtx -  is the cached version of the matrix, it's located in the parent (makeCacheMatrix) environment
##  civ -   is the cached version of the matrix, or NULL
##  inversion - is the computed inversion matrix passed as an argument in setinv
##  
makeCacheMatrix <- function(arg = matrix()) {   # Create an empty matrix in arg
    civ <- NULL                                 # civ s/b NULL until an inversion is computed
    cmtx <- matrix()                            # same for cmtx
#    
    set <- function(mtx) {                      # "set" function accepts a matrix from argument
        cmtx <<- mtx                            # Cache the matrix in the parent environment(makeCacheMatrix)
        civ <<- NULL                            # Cache NULL to civ in the parent environment
    }
    get <- function() {                         # "get" function has no argument
        cmtx                                    # It returns the matrix from cache in the parent enivronment
    }
    setinv <- function(inversion)  {            # "setinv" funcion accepts the inverse from an argument
        civ <<- inversion                       # Cache inverse into the parent environment
#       print(civ)                            ### Diagnostic - to remove before submission
        civ                                     # Returns the cached inverse from the parent environment
    }
    getinv <- function() {                      # "getinv" function has no argument
#       print(civ)                            ### Diagnostic - to remove before submission
        civ                                     # Return the inverse from cache in parent environment
    }
    list(set = set,                             # Putting names to functions in as list that
         get = get,                             # is returned by makeCacheMatrix, for later use
         setinv = setinv,
         getinv = getinv)
}                                               # Garbage collection eliminates local objects at this point


##  "cacheSolve" checks if an inverse is cached & either returns inverse from cache or from solve() 
##  If inverse is from solve(), then its cached and cache no longer NULL
##
##  When makeCacheMatrix is run, it will create a list of pointers to the functions it creates.  
##  This list is bound to the object name to which makeCacheMatrix's output is assigned
##  
##  When cacheSolve is called, it is passed the object name of the list of functions
##  arg -   is what we call the pointer name within cacheSolve
##  liv -   is the local object in the cacheSolve environment which contains NULL/the inverse
##  data -  is a local holder of the matrix used as the argument to solve()
## 
cacheSolve <- function(arg, mtx) {            # Pass function pointers to cacheSolve via arg
    liv <- arg$getinv()                       # Copy cached inverse/NULL to local inverse
    if(!is.null(liv))  {                      # !is.null means inversion has been calc'd
        message("getting cached inverse")
        return(liv)                           # Return ends the function, while returning liv
    }                                         # OTOH if the inverse is null, we continue the function
    hold <- arg$set(mtx)                      # Assing NULL to hold, while set caches the matrix
    data <- arg$get()                         # Assign matrix to data
    liv <- solve(data)                        # Invert the matrix into liv 
    arg$setinv(liv)                           # Push inversion to cache using liv as an argument
    liv                                       # return local copy of inversion
}                                             # At this point garbage collection eliminates local objects  

##  What follows are different steps to run and confirm the functions above
##

##  Create a matrix for the functions to play with/test
##  The matrix object is bound to the name mtrx
##
mtrx <-  matrix(c(-3, -1, 1, 1), 2,2)         


##  Establish the functions, accessable via zog a list of 4
##  Why not "zog", is more memorable than, for example, "x" 
##                                            # Create an instance of makeCacheMatrix functions (set,
zog <- makeCacheMatrix(arg)                   # get, setinv, getinv), store pointers in zog


##  Retreive inverse using cacheSolve
##  1st time through, the inverse s/b NULL, and cacheSolve will have to compute the inverse          
##                                            # "zulu" is an object where the inverse will be returned              
zulu <- cacheSolve(zog, mtrx)                 # "zog" passed as an argument to find pointers to functions
##                                            # The inverse will be returned into zulu from solve()


##  2nd time through, inverse should return from cache and message display confirming same  
##                                            # Same function, argument and object as above
zulu <- cacheSolve(zog)                       # but now the inverse is returned from cache (parent env) 
##  