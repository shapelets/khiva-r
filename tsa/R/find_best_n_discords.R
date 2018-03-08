#
# Copyright (c) 2018 Grumpy Cat Software S.L.
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
#
#' find_best_n_discords.R
#' @brief Primitive of the findBestNDiscords function.
#
#' @param profile The matrix profile containing the minimum distance of each
#' subsequence
#' @param index The matrix profile index containing where each minimum occurs
#' @param length_profile Length of the matrix profile
#' @param n Number of motifs to extract
#' @param motif_distances The distance of the best N discords
#' @param motif_indices The indices of the best N discords
#' @param subsequence_indices The indices of the query sequences that produced
#' the maximum reported in the discords
#
#' 
#' @return A list with the discor distances, the discord indices and the subsequences index.
#' @export

find_best_n_discords <- function(profile,index,n){
  
  shared_library <-system.file("extdata","libTSALIB.dylib",package="tsa")
  
  dyn.load(shared_library)
  
  library("bit64")
  
  try(
    out <- .C("find_best_n_discords",
              as.double(profile),
              as.integer(index),
              as.integer64(length(profile)),
              as.integer64(n),
              discord_distance = as.double(seq(length=n, from = 0, to = 0)),
              discord_index = as.integer(seq(length=n, from = 0, to = 0)),
              subsequence_index = as.integer(seq(length=n, from = 0, to = 0)),
              PACKAGE='libTSALIB.dylib')
  )
  
  newList <- list("discord_distance" = out$discord_distance,
                  "discord_index" = out$discord_index,
                  "subsequence_index" = out$subsequence_index)
  
  return(newList)
}