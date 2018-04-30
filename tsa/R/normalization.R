#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

#' Znorm
#' 
#' Calculates a new set of timeseries with zero mean and standard deviation one.
#' 
#' @param arr TSA Array with the time series.
#' @param epsilon Minimum standard deviation to consider.  It acts a a gatekeeper for
#' those time series that may be constant or near constant.
#' @return Array with the same dimensions as arr where the time series have been
#' adjusted for zero mean and one as standard deviation.
#' @export
Znorm <- function(arr, epsilon = 0.00000001) {
   
  try(out <-
        .C(
          "znorm",
          ptr = arr@ptr,
          as.double(epsilon),
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(createArray(out$b))
}