#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.
#
#
#'Stomp algorithm
#'STOMP algorithm to calculate the matrix profile between 'ta' and 'tb' using a subsequence length
#'of 'm'.
#' 
#'
#' 
#' @return A matrix profile 
#' @export 

stomp <- function(first_time_series, second_time_series, subsequence_length){
  
  shared_library <-system.file("extdata","libTSALIB.dylib",package="tsa")
  
  dyn.load(shared_library)
  
  library("bit64")

  try(
     out <- .C("stomp",
               as.double(first_time_series),
               as.double(second_time_series),
               as.integer(length(first_time_series)),
               as.integer(length(second_time_series)),
               as.integer64(subsequence_length),
               p = as.double(seq(length = (length(second_time_series) - subsequence_length + 1), from = 0, to = 0)),
               i = as.integer(seq(length = (length(second_time_series) - subsequence_length + 1), from = 0, to = 0)),
               PACKAGE='libTSALIB.dylib')
  )
  
  newList <- list("profile" = out$p, "index" = out$i)
  
  return(newList)
}