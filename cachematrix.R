## We define here two functions, that when combined, will deliver
## the inverse of a quadratic matrix, ie the number
## of rows is equal to the number of columns. The point here is
## to refer to the cached value when it has already been calculated.
## This will avoid, for example when embedded in a loop, to recalculate
## the inverse each run of the loop (we suppose that the matrix itself
## remains fixed).

## The makeCacheMatrix function takes as input a quadratic matrix and
## returns a list of three functions : 
## 1. The get() function will provide the matrix that has been delivered
## as input with the call of the makeCacheMatrix function
## 2. The setInverseM(par) function will assign the value of par to the 
## local variable m (local in the makeCacheMatrix function)
## 3. The getInverseM() function will be initialised with NULL, and once 
## the matrix inverse calculated, cache the calculated inverse. 

 
makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL
      ## m is a local variable in makeCacheMatrix
      
      get <- function() x
      ## When one runs the get function (use "get()"), it returns x
      
      setInverseM <- function(par) {m <<- par}  
      ## par is a formal parameter of the setInverse function.
      ## m is a local variable of the setInverseM function.
      ## setInverse(x) assigns the value x to m. The operator '<<-'
      ## ensures that the value of m is passed to the local 
      ## variable m of the makeCacheMMatrix function (or more precisely, 
      ## the instance of this funcion when it was invoked) and thus
      ## replaces the NULL initial value in this instance of the function.
      ## The new value of m will thus be picked up by the getCacheMatrix
      ## function. 
      ## That would not happen, if the '<-' operator had been used and the
      ## value of m in setmean remained purely local in that function. In that
      ## case, the makeVector function (or more precisely the instance of
      ## this function when it was invoked), would retain the NULL initialisation, 
      ## and getInverseM would thus still have the NULL value.
      
      getInverseM <- function() m
      ## The getInverseM function just takes the value m, when 
      ## you enter getInverseM()
      list(get = get, 
           setInverseM = setInverseM,
           getInverseM = getInverseM)
}


## The cacheSolve function takes as argument x the list provided
## by the makeCacheMatrix function. It returns a matrix that is 
## the inverse of the matrix that is provided with the instance of the
## makeCacheMatrix function (it is the x$get() function that provides 
## the user specified matrix)

cacheSolve <- function(x, ...) {
      m <- x$getInverseM()
      ## the above assignment calls from the makeCacheMatrix the value
      ## that is currently assigned to the getInverseM function.
      ## If it is different from NULL, its value will be maintained, 
      ## otherwise it will calculate the inverse and deliver it to the
      ## the instance of the makeCacheMatrix function through the
      ## x$setInverseM(m) function

      if(!is.null(m)) {
            message("getting cached inverse matrix")
            return(m)
      }
      data <- x$get()
      
      m <- solve(data, ...)
      
      ## The function 'solve' calculates the inverse of the matrix 'data' 
      x$setInverseM(m)
      m

}
