##Creates a special 'matrix' that can cache its inverse.
##Computes and retrieves the inverse from the cache.

## Write a short comment describing this function

makeCacheMatrix <- function(matr = matrix()) {
          ## matr : A square invertible matrix
          ## List containing functions to:
          ##                        1. set the matrix
          ##                        2. get the matrix       
          ##                        3. set the inverse  
          ##                        4. get the inverse

  inversed <- NULL         
  set <- function(y) { 
          ## use '<<-' assign a value to an object in an environment that is different from the current environment.
    matr <<- y
    inversed <<- NULL
  }
  
  get <- function() matr      
setinverse <- function(inverse) inversed <<- inverse  
getinverse <- function() inversed                   
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}



cacheSolve <- function(matr, ...) {
          ## @matr : output of makeCacheMatrix()
          ## return: inverse of the original matrix input 
          
  inversed <- matr$getinverse()
   
          # if the inverse has already been calculated
   if(!is.null(inversed)) {  
          # gets it from the cache and skips the computation.
          
          
     message("getting cached data")
     return(inversed)
   }
   
          # otherwise, calculates the inverse
   data <- matr$get()         
   inversed <- solve(data)
   
          # sets the value of the inverse in the cache.
   matr$setinverse(inversed)  
   
   inversed
}

## Example:
# matr <- makeCacheMatrix(matrix(1:4, 2, 2))
#> cacheSolve(matr)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

# matr$set(matrix(5:8, 2, 2))
# cacheSolve(matr)
#     [,1] [,2]
#[1,]   -4  3.5
#[2,]    3 -2.5
