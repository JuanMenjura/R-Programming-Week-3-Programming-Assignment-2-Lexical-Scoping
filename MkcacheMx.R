####################################################################################

MkcacheMx <- function(x = matrix()) {
  iv <- NULL
  st <- function(y) {
    x <<- y
    iv <<- NULL
  }
  obt <- function() {x}
  stinverse <- function(inverse) {iv <<- inverse}
  obtinverse <- function() {iv}
  list(st = st, obt = obt, stinverse = stinverse, obtinverse = obtinverse)
}

####################################################################################

cachesolve <- function(x, ...) {
  iv <- x$obtinverse()
  if(!is.null(iv)) {
    message("Getting cache data")
    return(iv)
  }
  mt <- x$obt()
  iv <- solve(mt, ...)
  x$stinverse(iv)
  iv
}

######################## JUAN MENJURA ##############################################

source("MkcacheMx.R")

test <- MkcacheMx(matrix(1:4, nrow = 2, ncol = 2))

test$obt()

test$obtinverse()

cachesolve(test)

cachesolve(test)

test$obtinverse()
