#' stamp algorithm
#'
#' This function executes an example of stamp algorithm.
#'
#' 
#' @return A matrix profile 
#' @export

stamp <- function(){
  shared_library <-system.file("extdata","libmylib-unified.dylib",package="tsa")
  print(shared_library)
  dyn.load(shared_library,package="a")
  a = as.integer(seq(length=20,from=0,to=0))
  b = as.double(seq(length=20,from=0,to=0))
  .C("get_info",package="a")
  #out <-.C("stamp",as.double(seq(length=30,from=1,to=15)),as.double(seq(length=30,from=6,to=15)),as.integer(10),as.integer(30),d=b,c=a, PACKAGE="b")
  #newList <- list("profile" = out$d, "index" = out$c)
  #return(newList)
}