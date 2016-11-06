## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
## It has four functions
## set - set the value of the matrix
## get - get the value of the matrix
## setInverse - set the value of the inverse of the matrix
## getInverse - get the value of the inverse matrix


## First two objects are initialised, x and c_inv

makeCacheMatrix <- function(x = matrix()) { # x initialised as 
                                            # a function argument
	
	c_inv <- NULL # c_inv initialised to NULL 
                  # within the function environment

## Define the 4 basic behaviours set, get, setInverse and getInverse
    set <- function(y) {
         
         x <<- y        # assign the input argument y 
                        # to the x object in the parent environmentthe object x is assigned a value
         
         c_inv <<- NULL # assign the value NULL to the c_inv object
                        # in the parent environment. (Deleting any cached value for c_inv if it exists)
    }
 
    get <- function() x # x is not defined in the function, get, so R, using lexical scoping 
                        # takes the value from the parent encvironment

    setInverse <- function(inverse) c_inv <<- inverse   # the function input argument, inverse, is used 
                                                        # to set the value of c_inv in the parent enviromnet 
                                                        # using the <<- operator

    getInverse <- function() c_inv      # c_inv is not defined in the function, getInverse, so R, using lexical scoping 
                                        # takes the value from the parent encvironment

## Each function is assigned to an element in a list. 
## The list is then returned from the function.
## This list then allows the convenient use of the $ extractor subsequently
    list(
    	set = set, # e.g this code gives the name "set" to the set function defined in the code above
    	get = get, 
        setInverse = setInverse, 
        getInverse = getInverse 
        )
  

}


## Write a short comment describing this function.
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache and 
## print a line "getting cached data from parent environment"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    inverse <- x$getInverse()   # assign the inverse of x using the getInverse behaviour from
                                # MakeCacheMatrix to the object inverse.
                                # Until cacheSolve is called and the inverse object filled 
                                # the x$getInverse value should be NULL

     if(!is.null(inverse)) {                                    # If it is not NULL then 
                                                                # a message is printed showing the 
                                                                # value is taken from the cache
         message("getting cached data from parent environment")
         return(inverse)
     }

     m_data <- x$get() # fills m_data with the matrix values from x

     inverse <- solve(m_data, ...)  # calculate the inverse of m_data using the solve function 
                                    # Computing the inverse of a square matrix can be done with the solve function 
                                    # in R. For example, if X is a square invertible matrix, 
                                    # then solve(X) returns its inverse.
                                    # It is assumed that x is always an invertible matrix

     x$setInverse(inverse) # sets the values of the inverse of the matrix x using the results of the solve funtion above

     inverse # returns the matrix "inverse" which is the inverse of the argument x

}
