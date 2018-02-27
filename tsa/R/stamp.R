#' stamp algorithm
#'
#' This function executes an example of stamp algorithm.
#'
#' 
#' @return A matrix profile 
#' @export

double_me <- function(){
  shared_library <-system.file("extdata","libmylib-unified.dylib",package="tsa")
  print(shared_library)
  dyn.load(shared_library)
  library("bit64")
  a <- as.double(c(1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10))
  b <- as.double(c(5,6,7,8,9,10,11,12,13,14,5,6,7,8,9,10,11,12,13,14))
  c <- as.integer64(5)
  d <- as.integer(20)
  e <- as.double(seq(length=15,from=0,to=0))
  f <- as.integer(seq(length=15,from=0,to=0))
  try(
     out <- .C("stamp",a,b,c,d,g=e,h=f,PACKAGE='libmylib-unified.dylib')
  )
  print(out$g)
  print(out$h)
  newList <- list("profile" = out$g, "index" = out$h)
  return(newList)
}