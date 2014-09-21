
## cache the inverse of the matrix so that it can be retrieved directly
## when caculating the inverse of the same matrix


## the function makeCacheMatrix creates an object that will cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    
    get<-function() x
    setsolve<-function(solve) m<<-solve
    getsolve<-function() m
    list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)

}


## the function cacheSolve will retrieve the inverse of the matrix if it
## exists in cache, otherwise, it will compute the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getsolve()
    if(!is.null(m)){
       
        return(m)
    }
    data<-x$get()
    m<-solve(data, ...)
    x$setsolve(m)
    m
}
