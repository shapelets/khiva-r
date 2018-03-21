#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

#' @brief Calculates de sum over the square values of the time series.
#'
#' @param  time.series List of time series double arrays.
#' @return List with the Absolute Energy.
#' @export
AbsEnergy <- function(time.series) {
  time.series.length <- as.integer64(length(time.series[[1]]))
  concatenated.time.series <-
    as.double(apply(cbind(time.series), 1, unlist))
  number.of.time.series <- as.integer64(length(time.series))
  
  try(out <- .C(
    "abs_energy",
    concatenated.time.series,
    time.series.length,
    number.of.time.series,
    result = as.double(seq(
      length = number.of.time.series,
      from = 0,
      to = 0
    )),
    PACKAGE = library
  ))
  
  newList <- list("result" = out$result)
  
  return(newList)
}

#' @brief Calculates the sum over the absolute value of consecutive
#' changes in the time series
#'
#' @param  time.series List of time.series double arrays.
#' @return List with the absoluteSumOfChanges
#' @export
AbsoluteSumOfChanges <- function(time.series) {
  time.series.length <- as.integer64(length(time.series[[1]]))
  concatenated.time.series <-
    as.double(apply(cbind(time.series), 1 , unlist))
  number.of.time.series <- as.integer64(length(time.series))
  
  try(out <- .C(
    "absolute_sum_of_changes",
    concatenated.time.series,
    time.series.length,
    number.of.time.series,
    result = as.double(seq(
      length = number.of.time.series,
      from = 0,
      to = 0
    )),
    PACKAGE = library
  ))
  
  newList <- list("result" = out$result)
  
  return(newList)
}

#' @brief Calculates the Schreiber, T. and Schmitz, A. (1997) measure of non-linearity
#' for the given time series
#'
#' @param time.series Time series.
#' @param lag The lag.
#' @return The non-linearity value for the given time series.
#' @export
C3 <- function(tss, lag) {
  tss.length <- as.integer64(length(tss[[1]]))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  tss.number.of.ts <- as.integer64(length(tss))
  
  try(out <- .C(
    "c3",
    tss.concatenated,
    tss.length,
    tss.number.of.ts,
    as.integer64(lag),
    result = as.double(seq(
      length = tss.number.of.ts,
      from = 0,
      to = 0
    )),
    PACKAGE = library
  ))
  
  r.result <- list("result" = out$result)
  
  return(r.result)
}

#' @brief Calculates an estimate for the time series complexity defined by
#' Batista, Gustavo EAPA, et al (2014). (A more complex time series has more peaks,
#' valleys, etc.)
#'
#' @param tss List of time series double arrays.
#' @param z.normalize Controls whether the time series should be z-normalized or not.
#' @return The complexity value for the given time series.
#' @export
CidCe <- function(tss, z.normalize) {
  tss.length <- as.integer64(length(tss[[1]]))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  tss.number.of.ts <- as.integer64(length(tss))
  
  try(out <- .C(
    "cidCe",
    tss.concatenated,
    tss.length,
    tss.number.of.ts,
    z.normalize,
    result = as.double(seq(
      length = tss.number.of.ts,
      from = 0,
      to = 0
    )),
    PACKAGE = library
  ))
  
  r.result <- list("result" = out$result)
  
  return(r.result)
}

#' @brief Calculates the cross-correlation of the given time series.
#'
#' @param xss Time series.
#' @param yss Time series.
#' @param unbiased Determines whether it divides by n - lag (if true) or n (if false).
#' @return The cross-correlation value for the given time series.
#' @export
CrossCorrelation <- function(xss, yss, unbiased) {
  xss.length <- as.integer64(length(xss[[1]]))
  xss.concatenated <- as.double(apply(cbind(xss), 1, unlist))
  xss.number.of.ts <- as.integer64(length(xss))
  
  yss.length <- as.integer64(length(yss[[1]]))
  yss.concatenated <- as.double(apply(cbind(yss), 1, unlist))
  yss.number.of.ts <- as.integer64(length(yss))
  
  try(out <- .C(
    "cross_correlation",
    xss.concatenated,
    xss.length,
    xss.number.of.ts,
    yss.concatenated,
    yss.length,
    yss.number.of.ts,
    unbiased,
    result = as.double(seq(
      length = max(xss.length, yss.length),
      from = 0,
      to = 0
    )),
    PACKAGE = library
  ))
  r.result <- list("result" = out$result)
  
  return(r.result)
}

#' @brief Calculates the cross-covariance of the given time series
#'
#' @param xss Time series.
#' @param yss Time series.
#' @param unbiased Determines whether it divides by n - lag (if true) or n (if false).
#' @return The cross-covariance value for the given time series.
#' @export
CrossCovariance <- function(xss, yss, unbiased) {
  xss.length <- as.integer64(length(xss[[1]]))
  xss.concatenated <- as.double(apply(cbind(xss), 1, unlist))
  xss.number.of.ts <- as.integer64(length(xss))
  
  yss.length <- as.integer64(length(yss[[1]]))
  yss.concatenated <- as.double(apply(cbind(yss), 1, unlist))
  yss.number.of.ts <- as.integer64(length(yss))
  
  try(out <- .C(
    "cross_covariance",
    xss.concatenated,
    xss.length,
    xss.number.of.ts,
    yss.concatenated,
    yss.length,
    yss.number.of.ts,
    unbiased,
    result = as.double(seq(
      length = (xss.length * yss.length),
      from = 0,
      to = 0
    )),
    PACKAGE = library
  ))
  r.result <- list("result" = out$result)
  
  return(r.result)
}

#' @brief Calculates the auto-covariance the given time series.
#'
#' @param xss Time series.
#' @param unbiased Determines whether it divides by n - lag (if true) or n (if false).
#' @return The auto-covariance value for the given time series.
#' @export
AutoCovariance <- function(xss, unbiased) {
  xss.length <- as.integer64(length(xss[[1]]))
  xss.concatenated <- as.double(apply(cbind(xss), 1, unlist))
  xss.number.of.ts <- as.integer64(length(xss))
  
  try(out <- .C(
    "auto_covariance",
    xss.concatenated,
    xss.length,
    xss.number.of.ts,
    unbiased,
    result = as.double(seq(
      length = (xss.length * xss.number.of.ts),
      from = 0,
      to = 0
    )),
    PACKAGE = library
  ))
  r.result <- list("result" = out$result)
  
  return(r.result)
}

#' @brief Calculates a vectorized Approximate entropy algorithm.
#' https://en.wikipedia.org/wiki/Approximate_entropy
#' For short time-series this method is highly dependent on the parameters, but should be stable for N > 2000,
#' see: Yentes et al. (2012) - The Appropriate Use of Approximate Entropy and Sample Entropy with Short Data Sets
#' Other shortcomings and alternatives discussed in:
#' Richman & Moorman (2000) - Physiological time-series analysis using approximate entropy and sample entropy
#'
#'
#' @param tss Time series.
#' @param m Length of compared run of data.
#' @param r Filtering level, must be positive.
#' @return The vectorized approximate entropy for all the input time series in tss.
#' @export
ApproximateEntropy <- function(xss, m, r) {
  xss.length <- as.integer64(length(xss[[1]]))
  xss.concatenated <- as.double(apply(cbind(xss), 1, unlist))
  xss.number.of.ts <- as.integer64(length(xss))
  
  try(out <- .C(
    "approximate_entropy",
    xss.concatenated,
    xss.length,
    xss.number.of.ts,
    as.integer(m),
    as.double(r),
    result = as.double(seq(
      length = xss.number.of.ts,
      from = 0,
      to = 0
    )),
    PACKAGE = library
  ))
  r.result <- list("result" = out$result)
  
  return(r.result)
}