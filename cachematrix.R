## These functions provide a more efficient handling of matrices especially when dealing with matrix inversion
## It creates a meta-class which store a matrix and methods to retrieve or set the matrix again
## Use --
## 1) Assign matrix to the meta-class via metamatrix <- makeCacheMatrix(matrix)
## 2) Get Matrix inverse via inverseMatrix <- cacheSolve(metamatrix)
## 3) Set or Get matrix stored in metamatrix via metamatrix$setMatrix(matrix) or metamatrix$getMatrix() respectively, when needed
## 
## _*Note :* Matrix inversion is only calculated once after each matrix reassignment and only when it is first needed._
## _*     :* Matrix is always assumed to be square and non singular_

## --------------------------------------------------------------------------------------

## function *makeCacheMatrix*
## args : matrix
## Creates meta-class of matrix for optimized inverse calculation.
## Inverse is only calculated when first needed.
## Cache is used during second inverse call and thereafter as long as the matrix is not changed via $setMatrix. 
## setMatrix will destroy the cache and the new inverse will only be calculated when needed
## *Note :* 1) matrix is always assumed to be square and non -singular, ie. have solution
##          2) setMatrixInverse is not built as it is not a safe mathematical operation. 
##             setMatrix will handle the Inverse calculation accordingly

makeCacheMatrix <- function(x = matrix()) {
    structure_id <- 0x01010110 # future use / please ignore

    inv_x <- NULL           # storing matrix inverse

    # Set a new matrix in the structure
    setMatrix <- function(y) {
        x <<- y             # update the new matrix value. Note that we are assigning to parent value
        inv_x <<- NULL      # reset matrix inverse. Note that we are assigning to parent value
        ## note : inverse is not calculated here since there is high possibility 
        ##        matrix are reassigned more frequently than inverse is needed.
    }

    # Return the current matrix stored in the structure
    getMatrix <- function () x
    
    
    # Return the matrix inverse. Inverse is calculated when first called and after each matrix reassignment
    # Matrix inverse is calculated here to provide consistency across library and to avoid initialization mistakes
    getMatrixInverse <- function() {
        if(is.null(inv_x)) {
            message("## Calculating matrix inverse and caching it")            
            inv_x <<- solve(x)  # Note that we are assigning to parent value
        } else {
            message("## Using cached matrix inverse.")            
            inv_x
        }
    }
    
    list(setMatrix = setMatrix, getMatrix = getMatrix, getMatrixInverse = getMatrixInverse, id = structure_id)
}

## --------------------------------------------------------------------------------------


## function *cacheSolve*
## args : matrix, ...
## Return inverse matrix 
## Note : Determination of whether returning cached value or not is made in makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv_x <- x$getMatrixInverse()
}


## -----------

testCacheSolve <- function() {
    message("Initializing matrix with a new value. ")
    m = matrix(rnorm(10000,0,1), 100,100)
    mm <- makeCacheMatrix(m)
    message("Message below should indicate matrix inverse is being calculated.")
    imm <- cacheSolve(mm)
    message("Message below should indicate cache is being used.")
    imm2 <- cacheSolve(mm)
    message("Reassigning matrix to a new value. ")
    n <- matrix(rnorm(100,0,2), 10,10)
    mm$setMatrix(n)
    message("Message below should indicate matrix inverse is being calculated.")
    imn <- cacheSolve(mm)
    message("Message below should indicate cache is being used.")
    imn2 <- cacheSolve(mm)
    ins <- solve(n)
    ims <- solve(m)

    
    message("Reassigning matrix to a new value. ")
    o <- matrix(rnorm(900,0,22), 30,30)
    mm$setMatrix(o)
    message("Message below should indicate matrix inverse is being calculated.")
    io <- cacheSolve(mm)
    message("Reassigning matrix to a new value. ")
    p <- matrix(rnorm(625,10,22), 25,25)
    mm$setMatrix(p)
    message("Message below should indicate matrix inverse is being calculated.")
    ip <- cacheSolve(mm)
    message("Message below should indicate cache is being used.")
    ip2 <- cacheSolve(mm)
    
    ios <- solve(o)
    ips <- solve(p)
    
    chksum = sum(imm-imm2)+sum(ims-imm)+sum(imn-imn2)+sum(ins-imn)+sum(ip-ip2)+sum(io-ios)+sum(ips-ip)
    
    if(chksum==0) {
        message("Test case result : PASSED")
    } else {
        warning("Test case result : FAILED. Checksum not null")
    }
    
    chksum
}