makeCacheMatrix <- function(x = matrix()) #creation of the function that will evaluate if the matrix already exist 
 {
        mymatrix <- NULL 
        set <- function(y) 
		{
                x <<- y #reassignation of "x"
                mymatrix <<- NULL
        	}
        get <- function() x #return the matrix specified by the user
        setinv <- function(inv) mymatrix <<- inv #here will be assigned the inverse of the matrix
        getinv <- function() mymatrix #if the matrix already has its inverse calculated, this will be return when the function cacheSolve runs
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv) #the components of the list
 }

cacheSolve <- function(x, ...) 
{  
        mymatrix <- x$getinv() #takes the value of the matrix created in the function makeCacheMatrix
        if(!is.null(mymatrix)) #if the inverse matrix has been calculated before,it will return that inverse
	{ 
                message("getting cached data")
                return(mymatrix) 
        }
        data <- x$get() #if the matrix specified by the user, doesn't have the inverse , this line will calculate it
        mymatrix <- solve(data, ...)
        x$setinv(mymatrix) 
        mymatrix #it will take the inverse matrix calculated
}
