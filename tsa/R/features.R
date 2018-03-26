#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

#' @brief Calculates de sum over the square values of the time series.
#'
#' @param  time.series List of arrays of type double containing the time series.
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
#' @param  time.series List of arrays of type double containing the time series.
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
#' @param time.series List of arrays of type double containing the time series.
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
#' @param tss List of arrays of type double containing the time series.
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
#' @param xss List of arrays of type double containing the time series.
#' @param yss List of arrays of type double containing the time series.
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
#' @param xss List of arrays of type double containing the time series.
#' @param yss List of arrays of type double containing the time series.
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
#' @param xss List of arrays of type double containing the time series.
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
#' @param tss List of arrays of type double containing the time series.
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

#' @brief Calculates the autocorrelation of the specified lag for the given time series.
#'
#' @param tss List of arrays of type double containing the time series.
#' @param max_lag The maximum lag to compute.
#' @param unbiased Determines whether it divides by n - lag (if true) or n (if false)
#' @return The autocorrelation value for the given time series.
#' @export
AutoCorrelation <- function(tss, max.lag, unbiased) {
  tss.length <- as.integer64(length(tss[[1]]))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  tss.number.of.ts <- as.integer64(length(tss))
  
  try(out <- .C(
    "auto_correlation",
    tss.concatenated,
    tss.length,
    tss.number.of.ts,
    as.integer64(max.lag),
    unbiased,
    result = as.double(seq(
      length = (tss.number.of.ts * tss.length),
      from = 0,
      to = 0
    )),
    PACKAGE = library
  ))
  
  return(out$result)
}

#' @brief Calculates the binned entropy for the given time series and number of bins.
#'
#' @param tss List of arrays of type double containing the time series.
#' @param max.bins The number of bins.
#' @return The binned entropy value for the given time series.
#' @export
BinnedEntropy <- function(tss, max.bins) {
  tss.length <- as.integer64(length(tss[[1]]))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  tss.number.of.ts <- as.integer64(length(tss))
  
  try(out <- .C(
    "binned_entropy",
    tss.concatenated,
    tss.length,
    tss.number.of.ts,
    as.integer(max.bins),
    result = as.double(seq(
      length = (tss.number.of.ts),
      from = 0,
      to = 0
    )),
    PACKAGE = library
  ))
  
  return(out$result)
}

#' @brief Calculates the number of values in the time series that are higher than
#' the mean.
#'
#' @param tss List of arrays of type double containing the time series.
#' @return The number of values in the time series that are higher than the mean.
#' @export
CountAboveMean <- function(tss) {
  tss.length <- as.integer64(length(tss[[1]]))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  tss.number.of.ts <- as.integer64(length(tss))
  
  try(out <- .C(
    "count_above_mean",
    tss.concatenated,
    tss.length,
    tss.number.of.ts,
    result = as.integer(seq(
      length = (tss.number.of.ts),
      from = 0,
      to = 0
    )),
    PACKAGE = library
  ))
  
  return(out$result)
}

#' @brief Calculates the number of values in the time series that are lower than
#' the mean
#'
#' @param tss List of arrays of type double containing the time series.
#' @return The number of values in the time series that are lower than the mean.
#' @export
CountBelowMean <- function(tss) {
  tss.length <- as.integer64(length(tss[[1]]))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  tss.number.of.ts <- as.integer64(length(tss))
  
  try(out <- .C(
    "count_below_mean",
    tss.concatenated,
    tss.length,
    tss.number.of.ts,
    result = as.integer(seq(
      length = (tss.number.of.ts),
      from = 0,
      to = 0
    )),
    PACKAGE = library
  ))
  
  return(out$result)
}

#' @brief Calculates the sum of squares of chunk i out of N chunks expressed as a ratio
#' with the sum of squares over the whole series. segmentFocus should be lower
#' than the number of segments.
#'
#' @param tss List of arrays of type double containing the time series.
#' @param num.segments The number of segments to divide the series into.
#' @param segment.focus The segment number (starting at zero) to return a feature on.
#' @return The energy ratio by chunk of the time series.
#' @export
EnergyRatioByChunks <- function(tss, num.segments, segment.focus) {
  tss.length <- as.integer64(length(tss[[1]]))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  tss.number.of.ts <- as.integer64(length(tss))
  
  try(out <- .C(
    "energy_ratio_by_chunks",
    tss.concatenated,
    tss.length,
    tss.number.of.ts,
    as.integer64(num.segments),
    as.integer64(segment.focus),
    result = as.double(seq(
      length = (tss.number.of.ts),
      from = 0,
      to = 0
    )),
    PACKAGE = library
  ))
  
  return(out$result)
}

#' @brief     Calculates the first relative location of the maximal value for each timeseries.
#'
#' @param tss List of arrays of type double containing the time series.
#' @return The first relative location of the maximum value to the length of the timeseries,
#' for each timeseries.
#' @export
#'
FirstLocationOfMaximum <- function(tss) {
  tss.length <- as.integer64(length(tss[[1]]))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  tss.number.of.ts <- as.integer64(length(tss))
  
  try(out <- .C(
    "first_location_of_maximum",
    tss.concatenated,
    tss.length,
    tss.number.of.ts,
    result = as.double(seq(
      length = (tss.number.of.ts),
      from = 0,
      to = 0
    )),
    PACKAGE = library
  ))
  
  return(out$result)
}

#' @brief     Calculates the first location of the minimal value of each time series. The position
#' is calculated relatively to the length of the series.
#'
#' @param tss List of arrays of type double containing the time series.
#' @return The first relative location of the minimal value of each series.
#' @export
#'
FirstLocationOfMinimum <- function(tss) {
  tss.length <- as.integer64(length(tss[[1]]))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  tss.number.of.ts <- as.integer64(length(tss))
  
  try(out <- .C(
    "first_location_of_minimum",
    tss.concatenated,
    tss.length,
    tss.number.of.ts,
    result = as.double(seq(
      length = (tss.number.of.ts),
      from = 0,
      to = 0
    )),
    PACKAGE = library
  ))
  
  return(out$result)
}

#' @brief Calculates if the input time series contain duplicated elements.
#'
#' @param tss List of arrays of type double containing the time series.
#' @return Array containing True if the time series contains duplicated elements
#' and false otherwise.
#' @export
#'
HasDuplicates <- function(tss) {
  tss.length <- as.integer64(length(tss[[1]]))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  tss.number.of.ts <- as.integer64(length(tss))
  
  try(out <- .C(
    "has_duplicates",
    tss.concatenated,
    tss.length,
    tss.number.of.ts,
    result = as.logical(seq(
      length = (tss.number.of.ts),
      from = 0,
      to = 0
    )),
    PACKAGE = library
  ))
  
  return(out$result)
}

#' @brief Calculates if the maximum within input time series is duplicated.
#'
#' @param tss List of arrays of type double containing the time series.
#' @return Array containing True if the maximum value of the time series is duplicated
#' and false otherwise.
#' @export
#'
HasDuplicateMax <- function(tss) {
  tss.length <- as.integer64(length(tss[[1]]))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  tss.number.of.ts <- as.integer64(length(tss))
  
  try(out <- .C(
    "has_duplicate_max",
    tss.concatenated,
    tss.length,
    tss.number.of.ts,
    result = as.logical(seq(
      length = (tss.number.of.ts),
      from = 0,
      to = 0
    )),
    PACKAGE = library
  ))
  
  return(out$result)
}

#' @brief Calculates the index of the max quantile.
#'
#' @param tss List of arrays of type double containing the time series.
#' @param q The quantile.
#' @return The index of the max quantile q.
#' @export
#'
IndexMaxQuantile <- function(tss, q) {
  tss.length <- as.integer64(length(tss[[1]]))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  tss.number.of.ts <- as.integer64(length(tss))
  
  try(out <- .C(
    "index_max_quantile",
    tss.concatenated,
    tss.length,
    tss.number.of.ts,
    as.double(q),
    result = as.double(seq(
      length = (tss.number.of.ts),
      from = 0,
      to = 0
    )),
    PACKAGE = library
  ))
  
  return(out$result)
}