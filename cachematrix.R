## These two functions cache the inverse of a matrix
## creates a matrix object that can cache its inverse
#The first function, makeVector creates a special "vector", which is really a list containing a function to - 

#1. set the value of the vector
#2. get the value of the vector
#3. set the value of the mean
#4. get the value of the mean
#set the value of the matrix
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    i<<-NULL
    x<<-y
  }
  get<-function()return(x)
  setinv<-function(inv) i<<-inv
  getinv<-function() return(i)
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)

}


#This function computes the inverse of the special matrix returned by 'makeCacheMatrix' above.
#3. set the value of the inverse
#4. get the value of the inverse
cacheSolve <- function(x, ...) {
  i<-x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  #x<-get()
  i<-solve(data,...)
  x$setinv(i)
  return(i)
}
