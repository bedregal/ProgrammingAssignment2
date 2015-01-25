## Assignment 2: The following two functions work together to calculate the inverse
##               of a matrix supplied by the user. Before the inverse is calculated
##               they checked if the inverse has been previously calculated for current 
##               input matrix and it is stored in the cache. If this is the case, the
##               inverse is just taken from the cache. If not, the inverse matrix is
##               calculated with the function SOLVE(), stored in the cache, and returned
##               to the user.
#########################################################################################



## (I) MAKECACHEMATRIX function: The input is the user-supplied matrix which is assumed to
##      be invertible. The output is a list containing 4 functions. These functions:
##          (1) Set the value of the matrix
##          (2) Get the value of the matrix
##          (3) Set the value of the INVERSE of the matrix
##          (4) Get the value of the INVERSE of the matrix
##

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        
        ## (1) Setting matrix
        set <- function(y) {
                x <<- y
                x_cache <<- y ## caching original matrix
                inv <<- NULL
        }
   
        
        ## (2) Getting matrix
        get <- function() x
        
        
        
        ## (3) Setting inverse matrix
        setinv <- function(inverse) {
                
                inv_cache <<- inverse
                
        }
        
        
        ## (4) Getting inverse matrix
        getinv <- function(m) {
                if(isTRUE(all.equal(m,x_cache)) == T) {
                       inv_cache       
                } 
                else {
                        set(m)
                        inv
                }
                
        }
        
        
        ## Output (a List)
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
        
}



## (2) CACHESOLVE function: It receives as arguments (i) a function (MAKECACHEMATRIX) 
##      and (ii) a matrix 'm' that is supposed to be invertible. It returns the inverse 
##      of matrix 'm'. 

cacheSolve <- function(fn=makeCacheMatrix, m=matrix()) {
        
        ## Checking if for matrix m we already have its inverse
        im <- fn$getinv(m)
        
        ## (a) If we  have the inverse, we just grab it from cache
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        
        ## (b) If we don't, we calculate it
        data <<- fn$get()
        im <- solve(data)
        fn$setinv(im)  ## Caching inverse matrix
        return(im)
              
}
