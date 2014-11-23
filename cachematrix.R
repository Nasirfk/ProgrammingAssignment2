## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

	## argument x is the matrix
	## Contains functions to:
	##	1. set the matrix (set)
        ##      2. get the matrix (get)
        ##      3. set the inverse (setinv)
        ##      4. get the inverse (getinv)
        
        inv = NULL 	#  inv will be our 'inverse' and it is reset to NULL every 
                 	#  time makeCacheMatrix is called


	## set funtion is not accessed by cacheSolve() but it could have been used during debugging
	## or possibly by another calling function other than cacheSolve().
	## It lets you assign a new value to the object
        
	set = function(y) {	# takes an input vector
                x <<- y		# use `<<-` to save input vector 
                inv <<- NULL	# resets the inverse to NULL, basically what happens when a new object is generated.
        }
        
	get = function() x	# this function returns the value of the original matrix
        setinv = function(inverse) {inv <<- inverse} 	# this is called by cacheSolve() during the first cacheSolve()
                                			#  access and it will store the value using superassignment

        getinv = function() inv				# this will return the cached value to cacheSolve() on
                                			#  subsequent accesses
        list(set=set, get=get, setinv=setinv, getinv=getinv)  	#  This is accessed each time makeCacheMatrix() is called,  
								#  that is, each time we make a new object.  This is a list of 
 								#  the internal functions ('methods') so a calling function
					                        #  knows how to access those methods.  

}


cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
	# the input x is an object created by makeCacheMatrix
	
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inv = x$getinv()			# accesses the object 'x' and gets the inverse
        
        
	if (!is.null(inv)){			# if the inverse has already been cached (not NULL)
		                		# get it from the cache and skips the computation. 
                message("getting cached data") 	# send this message to the console
                return(inv)			# and return the inverse, "return" ends the function cacheSolve()
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()			# we reach this code only if x$getinv() returned NULL
        inv = solve(mat.data, ...)		# if inv was NULL then we have to calculate the inverse using solve()
        
        
        x$setinv(inv)				# stores the value of the inverse in the cache via the setinv function.
        
        return(inv)				# return the inverse to the code that called this function
}
