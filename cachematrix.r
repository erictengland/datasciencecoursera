## CacheIndex is a variable that keeps track of how big the matrix cache is.
## The matrix cache is a list.  The list contains "sublists" of two elements each.
## Those "sublists" are the matrix followed by its inverse matrix

## When makeCacheMatrix is called, it takes the given matrix.
## It changes the index to reflect that the matrix cache is adding a matrix and
## its inverse.  Then it puts the matrix and its inverse as a list into the cache.
CacheIndex=0
MtxCache=list()
solution=matrix()
makeCacheMatrix <- function(x = matrix()) 
{
  CacheIndex <<- CacheIndex+1
  MtxCache[[CacheIndex]]<<- list(x, solution)
}


## cacheSolve first checks to see if there is a matrix cache.  If there isn't, 
## it automatically skips all other conditionals, finds the inverse of the matrix, 
## caches the matrix and the computes inverse, and returns the inverse.  When 
## there is a cache, it cycles through it to check if the inverse of matrix
## has been requested before.  It will then retrieve or solve then cache.
## print functions have been commented out, but may be uncommented to display
## results if the functions are not being run directly from the console.

cacheSolve <- function(x, ...) 
{
    
  if (CacheIndex > 0)
  for (i in 1:CacheIndex)
  {
    if (identical(MtxCache[[i]][[1]],x))
    {
      idx <<- i
      #print(MtxCache[[idx]][[2]])
      return(MtxCache[[idx]][[2]])
    }
  }
  solution<<-solve(x)
  makeCacheMatrix(x)
  #print(solution)
  return(solution)
}
