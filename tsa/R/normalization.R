#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

#' @brief Calculates a new set of timeseries with zero mean and standard deviation one.
#' @param tss List of arrays of type double containing the time series.
#' @param epsilon Minimum standard deviation to consider.  It acts a a gatekeeper for
#' those time series that may be constant or near constant.
#' @return Array with the same dimensions as tss where the time series have been
#' adjusted for zero mean and one as standard deviation.
#' @export
Znorm <- function(tss, epsilon = 0.00000001) {
  tss_l <- as.integer64(length(tss[[1]]))
  tss_n <- as.integer64(length(tss))
  tssConcatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  try(out <-
        .C(
          "znorm",
          tssConcatenated,
          tss_l,
          tss_n,
          as.double(epsilon),
          result = as.double(seq(
            length = (tss_n * tss_l),
            from = 0,
            to = 0
          )),
          PACKAGE = library
        ))
  return(out$result)
}
