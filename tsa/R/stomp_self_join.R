#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.
#
#' Stomp self join algorithm
#'STOMP algorithm to calculate the matrix profile between 't' and itself using a subsequence length
#'of 'm'. This method filters the trivial matches.
#' 
#'
#' 
#' @return A matrix profile 
#' @export

StompSelfJoin <- function(first.time.series,subsequence.length){
  
  shared.library <-system.file("extdata","libTSALIB.dylib",package="tsa")
  
  dyn.load(shared.library)
  
  library("bit64")
  
  try(
    out <- .C("stomp_self_join",
              as.double(first.time.series),
              as.integer(length(first.time.series)),
              as.integer64(subsequence.length),
              p = as.double(seq(length=(length(first.time.series) - subsequence.length + 1), from = 0, to = 0)),
              i = as.integer(seq(length=(length(first.time.series) - subsequence.length + 1), from = 0, to = 0)),
              PACKAGE='libTSALIB.dylib')
  )
  
  newList <- list("profile" = out$p, "index" = out$i)
  
  return(newList)
}