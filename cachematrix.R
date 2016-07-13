## The functions aim to store the inverse of a matrix in the cache, this result
## can be used as required and avoids the need for the inverse to be calculated multiple times

## This function creates a vector containing four additional functions and their results
makeCacheMatrix <- function(Specified_Matrix = matrix()) {
        
        #Set the temporary inverse parameter to NULL if makeCacheMatrix called
        Inverse_Param <- NULL
        
        setMatrix <- function(Specified_Matrix) {
                #Set the cached matrix to the specified matrix
                Cached_Matrix <<- Specified_Matrix
                #Set the cached inverse parameter to NULL if set called as the inverse has not yet been calculated
                Inverse_Param <<- NULL
        }
        
        getMatrix <- function(){
                #Return the matrix
                Specified_Matrix
        }
        
        setInverse <- function(Inverse) {
                #Set the cached inverse if setInverse called
                Inverse_Param <<- Inverse
        }
        
        getInverse <- function(){
                #Return the cached inverse if getInverse called
                Inverse_Param
        }
        
        #Name the objects in our function list
        list(set = setMatrix, get = getMatrix,setInverse = setInverse, getInverse = getInverse)
}


##For the specified input vector created in makeCacheMatrix this function returns the inverse
##of the input matrix, but checks for the inverse in the cache and wiull return this if available
##if not the inverse will be explicitally calculated

cacheSolve <- function(Input, ...) {
        ## Return a matrix that is the inverse of 'Input'
        
        #set Temp_Inverse to the the cached inverse
        Temp_Inverse <- Input$getInverse()
        
        #Check to see if the inverse has already been calculated and cached
        if(!is.null(Temp_Inverse)) {
                #If yes return it rather than calculate it
                message("getting cached data")
                #Return the result
                return(Temp_Inverse)
        }
        
        #Otherwise
        
        #return the cached matrix
        data <- Input$get()
        
        #get the inverse of the cached matrix
        Temp_Inverse <- solve(data, ...)
        
        #Set the cached inverse to the calculated for later use
        Input$setInverse(Temp_Inverse)
        
        #print the new inverse to the console
        Temp_Inverse
}


