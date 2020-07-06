## MakeCacheMatrix makes a list with the setters and getters and initialises
##values of inverse and other free variables.

##cacheSolve computes the inverse or returns the cached value of inverse if 
##if the inversse has already been computed berfore on the same matrix



makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  settinginverse<-function(inverse) i<<-inverse
  gettinginverse<-function()i
  list(set=set,get=get,
       settinginverse=settinginverse,
       gettinginverse=gettinginverse)
  
}


## Returns the inverse (returns the cached value or computes(if not computed before))

cacheSolve <- function(x,...) {
  i<-x$gettinginverse()
  if (!is.null(i)){
    message('Cached Inverse Available')
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$settinginverse(i)
  i
}
