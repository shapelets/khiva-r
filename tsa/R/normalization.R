#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

#' DecimalScalingNorm
#'
#' Normalizes the given time series according to its maximum value and adjusts each value within the range (-1, 1).
#'
#' @param tss: TSA array with the time series.
#' @return: TSA array with the same dimensions as tss, whose values (time series in dimension 0) have been
#' normalized by dividing each number by 10^j, where j is the number of integer digits of the max number in
#' the time series.
#' @export
DecimalScalingNorm <- function(tss) {
  try(out <-
        .C(
          "decimal_scaling_norm",
          ptr = tss@ptr,
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(tss@ptr <- out$ptr))
  return(createArray(out$b))
}

#' DecimalScalingNormInPlace
#'
#' Same as decimal_scaling_norm, but it performs the operation in place, without allocating further memory.
#'
#' @param tss: TSA array with the time series.
#' @export
DecimalScalingNormInPlace <- function(tss) {
  try(out <-
        .C("decimal_scaling_norm_in_place",
           ptr = tss@ptr,
           PACKAGE = package))
  eval.parent(substitute(tss@ptr <- out$ptr))
}

#' MaxMinNorm
#'
#' ormalizes the given time series according to its minimum and maximum value and adjusts each value within the
#' range [low, high].

#' @param tss: TSA array with the time series.
#' @param high: Maximum final value (Defaults to 1.0).
#' @param low: Minimum final value (Defaults to 0.0).
#' @param epsilon: Safeguard for constant (or near constant) time series as the operation implies a unit scale
#' operation between min and max values in the tss.
#' @return: TSA array with the same dimensions as tss where the time series have been adjusted for zero mean and
#' one as standard deviation.
#' @export
MaxMinNorm <-
  function(tss,
           high = 1.0,
           low = 1.0,
           epsilon = 0.00000001) {
    try(out <-
          .C(
            "max_min_norm",
            ptr = tss@ptr,
            as.double(high),
            as.double(low),
            as.double(epsilon),
            b = as.integer64(0),
            PACKAGE = package
          ))
    eval.parent(substitute(tss@ptr <- out$ptr))
    return(createArray(out$b))
  }

#' MaxMinNormInPlace
#'
#' Same as max_min_norm, but it performs the operation in place, without allocating further memory.
#'
#' @param tss: TSA array with the time series.
#' @param high: Maximum final value (Defaults to 1.0).
#' @param low: Minimum final value (Defaults to 0.0).
#' @param epsilon: Safeguard for constant (or near constant) time series as the operation implies a unit scale
#' operation between min and max values in the tss.
#' @export
MaxMinNormInPlace <-
  function(tss,
           high = 1.0,
           low = 1.0,
           epsilon = 0.00000001) {
    try(out <-
          .C(
            "max_min_norm_in_place",
            ptr = tss@ptr,
            as.double(high),
            as.double(low),
            as.double(epsilon),
            PACKAGE = package
          ))
    eval.parent(substitute(tss@ptr <- out$ptr))
  }

#' MeanNorm
#'
#' Normalizes the given time series according to its maximum-minimum value and its mean. It follows the following
#'formulae:
#' \deqn{ \acute{x} = \frac{x - mean(x)}{max(x) - min(x)}.}
#'
#' @param tss TSA array with the time series.
#' @return An array with the same dimensions as tss, whose values (time series in dimension 0) have been
#' normalized by substracting the mean from each number and dividing each number by \eqn{max(x) - min(x)}, in the
#' time series.
#' @export
MeanNorm <- function(tss) {
  try(out <- .C("mean_norm",
                ptr = tss@ptr,
                b = as.integer64(0),
                PACKAGE = package))
  eval.parent(substitute(tss@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' MeanNormInPlace
#'
#' Normalizes the given time series according to its maximum-minimum value and its mean. It follows the following
#' formulae: 
#' \deqn{ \acute{x} = \frac{x - mean(x)}{max(x) - min(x)}.}
#'
#' @param tss TSA array with the time series.
#' @export
MeanNormInPlace  <- function(tss) {
  try(out <- .C("mean_norm_in_place",
                ptr = tss@ptr,
                PACKAGE = package))
  eval.parent(substitute(tss@ptr <- out$ptr))
}

#' Znorm
#'
#' Calculates a new set of time series with zero mean and standard deviation one.
#'
#' @param arr TSA Array with the time series.
#' @param epsilon Minimum standard deviation to consider. It acts a a gatekeeper for
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

#' ZnormInPLace
#'
#' Adjusts the time series in the given input and performs z-norm
#' inplace (without allocating further memory).
#'
#' @param arr: TSA array with the time series.
#' @param epsilon: epsilon Minimum standard deviation to consider. It acts as a gatekeeper for
#' those time series that may be constant or near constant.
#' @export
ZnormInPlace <- function(arr, epsilon = 0.00000001) {
  try(out <-
        .C(
          "znorm_in_place",
          ptr = arr@ptr,
          as.double(epsilon),
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(createArray(out$b))
}