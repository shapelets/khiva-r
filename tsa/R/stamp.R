#'Stamp algorithm
#'STAMP algorithm to calculate the matrix profile between 'ta' and 'tb' using a subsequence length
#'of 'm'.
#' 
#'
#' 
#' @return A matrix profile 
#' @export 

stamp <- function(first_time_series, second_time_series, subsequence_length){
  
  shared_library <-system.file("extdata","libTSALIB.0.0.1.dylib",package="tsa")
  
  dyn.load(shared_library)
  
  library("bit64")

  try(
     out <- .C("stamp",
               as.double(first_time_series),
               as.double(second_time_series),
               as.integer(length(first_time_series)),
               as.integer(length(second_time_series)),
               as.integer64(subsequence_length),
               p = as.double(seq(length = (length(second_time_series) - subsequence_length + 1), from = 0, to = 0)),
               i = as.integer(seq(length = (length(second_time_series) - subsequence_length + 1), from = 0, to = 0)),
               PACKAGE='libTSALIB.0.0.1.dylib')
  )
  
  newList <- list("profile" = out$p, "index" = out$i)
  
  return(newList)
}