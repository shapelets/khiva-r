#
#Copyright (c) 2018 Shapelets.io
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

#' Stomp
#'
#' STOMP algorithm to calculate the matrix profile between 'first.time.series' and 'second.time.series' using a subsequence length
#' of 'subsequence.length'.
#'
#' @param first.time.series KHIVA Array the time series.
#' @param second.time.series KHIVA Array time series.
#' @param subsequence.length Length of the subsequence.
#' @return List of KHIVA Arrays with the matrix profile and the index profile
#' @export
Stomp <-
  function(first.time.series,
           second.time.series,
           subsequence.length) {
    try(out <- .C(
      "stomp",
      f.ptr = first.time.series@ptr,
      s.ptr = second.time.series@ptr,
      as.integer64(subsequence.length),
      profile = as.integer64(0),
      index = as.integer64(0),
      PACKAGE = package
    ))
    eval.parent(substitute(first.time.series@ptr <- out$f.ptr))
    eval.parent(substitute(second.time.series@ptr <- out$s.ptr))
    
    newList <-
      list("profile" = createArray(out$profile),
           "index" = createArray(out$index))
    
    return(newList)
  }

#' StompSelfJoin
#'
#' STOMP algorithm to calculate the matrix profile between 't' and itself using a subsequence length
#' of 'm'. This method filters the trivial matches.
#'
#' @param t KHIVA Array the time series.
#' @param m KHIVA Array the time series.
#' @return List of KHIVA Arrays with the matrix profile and the index profile
#' @export
StompSelfJoin <- function(t, m) {
  try(out <- .C(
    "stomp_self_join",
    ptr = t@ptr,
    as.integer64(m),
    p = as.integer64(0),
    i = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(t@ptr <- out$ptr))
  
  newList <-
    list("profile" = createArray(out$p), "index" = createArray(out$i))
  
  return(newList)
}

#' FindBestNDiscords
#'
#' This function extracts the best N discords from a previously calculated matrix profile.
#
#' @param profile KHIVA Array with the matrix profile containing the minimum distance of each
#' subsequence.
#' @param index KHIVA Array with the matrix profile index containing where each minimum occurs.
#' @param m Subsequence length value used to calculate the input matrix profile.
#' @param n Number of motifs to extract.
#' @param selfJoin Indicates whether the input profile comes from a self join operation or not. It determines 
#' whether the mirror similar region is included in the output or not.
#' @return A list of KHIVA Arrays with the discord distances, the discord indices and the subsequences indices.
#' @export
FindBestNDiscords <- function(profile, index, m, n, selfJoin) {
  try(out <- .C(
    "find_best_n_discords",
    p.ptr = profile@ptr,
    i.ptr = index@ptr,
    as.integer64(m),
    as.integer64(n),
    discord.distance = as.integer64(0),
    discord.index = as.integer64(0),
    subsequence.index = as.integer64(0),
    selfJoin,
    PACKAGE = package
  ))
  
  newList <- list(
    "discord.distance" = createArray(out$discord.distance),
    "discord.index" = createArray(out$discord.index),
    "subsequence.index" = createArray(out$subsequence.index)
  )
  eval.parent(substitute(profile@ptr <- out$p.ptr))
  eval.parent(substitute(index@ptr <- out$i.ptr))
  
  return(newList)
}

#' FindBestNMotifs
#'
#' This function extracts the best N motifs from a previously calculated matrix profile.
#
#' @param profile KHIVA Array with the matrix profile containing the minimum distance of each
#' subsequence.
#' @param index KHIVA Array with the matrix profile index containing where each minimum occurs.
#' @param m Subsequence length value used to calculate the input matrix profile.
#' @param n Number of motifs to extract
#' @param selfJoin Indicates whether the input profile comes from a self join operation or not. It determines 
#' whether the mirror similar region is included in the output or not.
#' @return A list of KHIVA Arrays with the motif distance, the motif indices and the subsequence indices.
#' @export
FindBestNMotifs <- function(profile, index, m, n, selfJoin) {
  try(out <- .C(
    "find_best_n_motifs",
    p.ptr = profile@ptr,
    i.ptr = index@ptr,
    as.integer64(m),
    as.integer64(n),
    motif.distance = as.integer64(0),
    motif.index = as.integer64(0),
    subsequence.index = as.integer64(0),
    selfJoin,
    PACKAGE = package
  ))
  
  newList <- list(
    "motif.distance" = createArray(out$motif.distance),
    "motif.index" = createArray(out$motif.index),
    "subsequence.index" = createArray(out$subsequence.index)
  )
  eval.parent(substitute(profile@ptr <- out$p.ptr))
  eval.parent(substitute(index@ptr <- out$i.ptr))
  
  return(newList)
}