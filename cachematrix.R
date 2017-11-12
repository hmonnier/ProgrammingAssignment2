
## This function îs used to cache/calculate the inverse of a matrix
## Set      :   Initalze the matrix for the all function (matrix == Argument of the function).
##              Populate the variable in a environnenment different than the current environnement (using <<- operator)
## Get      :   Retrieve the Matrix
## SetInv   :   Calculate the inverse matrix
## GetInv   :   Retrieve the inverse matrix
makeCacheMatrix <- function(x = matrix()) {

    invM <- NULL                    # Init var
    
    SetM <- function(z) {           # Init Matrix
            x <<- z
            invM <<- NULL           # Init Var
           }
    
    GetM    <- function () x                  # Allow to get the matrix
    
    SetInvM <- function (inv) invM <<- inv    # Calculte the inverse of the matrix
    
    GetInvM <- function () invM               # Allow to get the inverse matrix
    
    list (SetM = SetM, GetM = GetM, SetInvM = SetInvM, GetInvM = GetInvM)
    
}


## This function calculates the inverse of a matrix. If the inverse exist in cache, returns the cached inverse matrix,
## else calculate the inverse and returns the inverse matrix (by solve function)

cacheSolve <- function(x, ...) {

    invM <- x$GetInvM()                         # evaluate value of the inverse matrix!! The variable value is present via <<- operator
    if(!is.null(invM)) {                        # the inverse is not null so it exists in cache and returned
          message("Matrix from cache")   
          return(invM)                          # return stop the function  
    }
          
    InitM <- x$GetM()                           # Init the original matrix 
    invM <- solve(InitM, ...)                   # Calculte inverse matrix
    x$SetInvM(invM)                             # Postulate the resukt 
    return(invM)                               
}
