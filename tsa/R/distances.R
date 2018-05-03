#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

#' Euclidean
#'
#' Calculates euclidean distances between time series.
#'
#' @param tss Expects an input array whose dimension zero is the length of the time series (all the same) and
#' dimension one indicates the number of time series.
#' @return TSA array with an upper triangular matrix where each position corresponds to the distance between two
#' time series. Diagonal elements will be zero. For example: Position row 0 column 1 records the distance
#' between time series 0 and time series 1.
#' @export
Euclidean <- function(tss) {
  try(out <- .C("euclidean",
                ptr = tss@ptr,
                b = as.integer64(0),
                PACKAGE = package))
  eval.parent(substitute(tss@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' SquaredEuclidean
#'
#' Calculates non squared version of the euclidean distance.
#'
#' @param tss Expects an input array whose dimension zero is the length of the time series (all the same) and
#' dimension one indicates the number of time series.
#' @return TSA array with an upper triangular matrix where each position corresponds to the distance between two
#' time series. Diagonal elements will be zero. For example: Position row 0 column 1 records the distance
#' between time series 0 and time series 1.
#' @export
SquaredEuclidean <- function(tss) {
  try(out <- .C("squared_euclidean",
                ptr = tss@ptr,
                b = as.integer64(0),
                PACKAGE = package))
  eval.parent(substitute(tss@ptr <- out$ptr))
  
  return(createArray(out$b))
}