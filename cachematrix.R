## Put comments here that give an overall description of what your
## functions do

## Custom matrix that has 4 functions.
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of matrix (inverse)
# 4. get the value of matrix (inverse)
makeCacheMatrix <- function(matrixNormal = matrix()) {
	matrixInverse <- NULL
	set <- function(matrixNormalParam) {
		matrixNormal <<- matrixNormalParam
		matrixInverse <<- NULL
	}
	get <- function() matrixNormal
	setInverse <- function(matrixInverseParam) {
		matrixInverse <<- matrixInverseParam
	}
	getInverse <- function() matrixInverse
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Retrieves inverse matrix from cache. else put to cache using setInverse function
cacheSolve <- function(customMatrix, ...) {
		matrixInverse <- customMatrix$getInverse()

		if(!is.null(matrixInverse)) {
			message("Getting cached data");
			return(matrixInverse)
		}	
		
		matrixNormal <- customMatrix$get()
		matrixInverse <- solve(matrixNormal)
		customMatrix$setInverse(matrixInverse)
		matrixInverse
}



####### Sample Output:
#### Default Matrix 
# > defaultMatrix <- matrix(1:4, 2, 2)
# > defaultMatrix
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > solve(defaultMatrix)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5


#### Custom Matrix (Caches)
# > customMatrix <- makeCacheMatrix(defaultMatrix)
# > customMatrix$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(customMatrix)
#     [,1] [,2]							<--------------- Not yet cached
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(customMatrix)
# Getting cached data					<--------------- Cached
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5