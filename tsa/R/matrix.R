#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

#' Stomp
#'
#' STOMP algorithm to calculate the matrix profile between 'ta' and 'tb' using a subsequence length
#' of 'm'.
#'
#' @param firs.time.series List of arrays of type double containing the time series. 
#' @param second.time.series List of arrays of type double containing the time series. 
#' @param subsequence.lenth Length of the subsequence.
#' @return A matrix profile
#' @export

Stomp <-
  function(first.time.series,
           second.time.series,
           subsequence.length) {
    try(out <- .C(
      "stomp",
      as.double(first.time.series),
      as.double(second.time.series),
      as.integer64(length(first.time.series)),
      as.integer64(length(second.time.series)),
      as.integer64(subsequence.length),
      p = as.double(seq(
        length = (length(second.time.series) - subsequence.length + 1),
        from = 0,
        to = 0
      )),
      i = as.integer(seq(
        length = (length(second.time.series) - subsequence.length + 1),
        from = 0,
        to = 0
      )),
      PACKAGE = package
    ))
    
    newList <- list("profile" = out$p, "index" = out$i)
    
    return(newList)
  }

#' StompSelfJoin
#' 
#' STOMP algorithm to calculate the matrix profile between 't' and itself using a subsequence length
#' of 'm'. This method filters the trivial matches.
#'
#' @param first.time.series List of arrays of type double containing the time series. 
#' @param subsequence.length Lenght of the subsequence
#' @return A matrix profile
#' @export

StompSelfJoin <- function(first.time.series, subsequence.length) {
  try(out <- .C(
    "stomp_self_join",
    as.double(first.time.series),
    as.integer64(length(first.time.series)),
    as.integer64(subsequence.length),
    p = as.double(seq(
      length = (length(first.time.series) - subsequence.length + 1),
      from = 0,
      to = 0
    )),
    i = as.integer(seq(
      length = (length(first.time.series) - subsequence.length + 1),
      from = 0,
      to = 0
    )),
    PACKAGE = package
  ))
  
  newList <- list("profile" = out$p, "index" = out$i)
  
  return(newList)
}

#' FindBestNDiscords
#' 
#' This function extracts the best N discords from a previously calculated matrix profile.
#
#' @param profile The matrix profile containing the minimum distance of each
#' subsequence.
#' @param index The matrix profile index containing where each minimum occurs.
#' @param n Number of motifs to extract.
#' @return A list with the discor distances, the discord indices and the subsequences indices.
#' @export

FindBestNDiscords <- function(profile, index, n) {
  try(out <- .C(
    "find_best_n_discords",
    as.double(profile),
    as.integer(index),
    as.integer64(length(profile)),
    as.integer64(n),
    discord.distance = as.double(seq(
      length = n,
      from = 0,
      to = 0
    )),
    discord.index = as.integer(seq(
      length = n,
      from = 0,
      to = 0
    )),
    subsequence.index = as.integer(seq(
      length = n,
      from = 0,
      to = 0
    )),
    PACKAGE = package
  ))
  
  newList <- list(
    "discord.distance" = out$discord.distance,
    "discord.index" = out$discord.index,
    "subsequence.index" = out$subsequence.index
  )
  
  return(newList)
}

#' FindBestNMotifs
#' 
#' This function extracts the best N motifs from a previously calculated matrix profile.
#
#' @param profile The matrix profile containing the minimum distance of each
#' subsequence
#' @param index The matrix profile index containing where each minimum occurs
#' @param length_profile Length of the matrix profile
#' @param n Number of motifs to extract
#' @return A list with the motif distance, the motif indices and the subsequence indices.
#' @export

FindBestNMotifs <- function(profile, index, n) {
  try(out <- .C(
    "find_best_n_motifs",
    as.double(profile),
    as.integer(index),
    as.integer64(length(profile)),
    as.integer64(n),
    motif.distance = as.double(seq(
      length = n,
      from = 0,
      to = 0
    )),
    motif.index = as.integer(seq(
      length = n,
      from = 0,
      to = 0
    )),
    subsequence.index = as.integer(seq(
      length = n,
      from = 0,
      to = 0
    )),
    PACKAGE = package
  ))
  
  newList <- list(
    "motif.distance" = out$motif.distance,
    "motif.index" = out$motif.index,
    "subsequence.index" = out$subsequence.index
  )
  
  return(newList)
}