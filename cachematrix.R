
## R course assignment 2, week 3.

## This function creates a name out of the (up to) 100 first entries in the matrix and
## saves the inverted matrix i the environment under that name.

makeCacheMatrix <- function(x = matrix()) {
  max_len <- min(length(x),100)                            # Find the maximum length for the name.
  name <- paste("inv",x[1:max_len],collapse="", sep="")    # Create the name of the inverted matrix.
  assign(paste(name),solve(x), envir=.GlobalEnv)           # Assign the inverted matrix to the name.
  return(name)                                             # Return the inverted matrix.
}

## This function finds out if there exists an object called by the name as specified above, 
## and if ther eis a such, it returns it. If not, it calculates the inverted object and
## returns it.

cacheSolve <- function(x, ...) {
  max_len <- min(length(x),100)                           # Find the maximum length for the name.
  name <- paste("inv",x[1:max_len],collapse="", sep="")   # Create the name of the inverted matrix.
  if(exists(paste(name)))                                 # If the name exists in the environment..
  {
    y <- get(name)                                        # Then get it from the cache
    message("getting cached data") 
  }
  else                                                    # And if it doesn´t exist...
  {
    message("inverting matrix")
    y <- solve(x)                                         # Then calculate the inverted matrix
  }
  return(y)
}

#################### notes for help: #####################

#testmat <- matrix(c(1,2,3,4),nrow=2)
#testmat2 <- testmat*1.1
#testmat3 <- testmat*1.2
#testmat4 <- testmat*1.3
#testmat5 <- testmat*1.4

#makeCacheMatrix(testmat)
#makeCacheMatrix(testmat3)
#makeCacheMatrix(testmat2)
#makeCacheMatrix(testmat4)
#makeCacheMatrix(testmat5)

#cacheSolve(testmat)
#cacheSolve(testmat2)
#cacheSolve(testmat3)
#cacheSolve(testmat4)
#cacheSolve(testmat5)

