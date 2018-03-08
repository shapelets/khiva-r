#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.
#
#' find_best_n_motifs.R
#' @brief Primitive of the findBestNMotifs function.
#
#' @param profile The matrix profile containing the minimum distance of each
#' subsequence
#' @param index The matrix profile index containing where each minimum occurs
#' @param length_profile Length of the matrix profile
#' @param n Number of motifs to extract
#'
#
#' 
#' @return A list with the motif distance, the motif indices and the subsequence indices.
#' @export

FindBestNMotifs <- function(profile,index,n){
  
  shared.library <-system.file("extdata","libTSALIB.dylib",package="tsa")
  
  dyn.load(shared.library)
  
  library("bit64")
  
  try(
    out <- .C("find_best_n_motifs",
              as.double(profile),
              as.integer(index),
              as.integer64(length(profile)),
              as.integer64(n),
              motif.distance = as.double(seq(length=n, from = 0, to = 0)),
              motif.index = as.integer(seq(length=n, from = 0, to = 0)),
              subsequence.index = as.integer(seq(length=n, from = 0, to = 0)),
              PACKAGE='libTSALIB.dylib')
  )
  
  newList <- list("motif.distance" = out$motif.distance,
                  "motif.index" = out$motif.index,
                  "subsequence.index" = out$subsequence.index)
  
  return(newList)
}