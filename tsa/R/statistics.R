#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

#' CovarianceStatistics
#'
#' Returns the covariance matrix of the time series contained in tss.
#'
#' @param tss Expects an input array whose dimension zero is the length of the time series (all the same) and
#' dimension one indicates the number of time series.
#' @param unbiased Determines whether it divides by n -1 (if false) or n (if true).
#' @return The covariance matrix of the time series.
#' @export
CovarianceStatistics <- function(tss, unbiased) {
  try(out <- .C(
    "covariance_statistics",
    ptr = tss@ptr,
    as.logical(unbiased),
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(tss@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' MomentStatistics
#'
#' Returns the kth moment of the given time series.
#'
#' @param tss Expects an input array whose dimension zero is the length of the time series (all the same) and
#' dimension one indicates the number of time series.
#' @param k The specific moment to be calculated.
#' @return The kth moment of the given time series.
#' @export
MomentStatistics <- function(tss, k) {
  try(out <- .C(
    "moment_statistics",
    ptr = tss@ptr,
    as.integer(k),
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(tss@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' SampleStdevStatistics
#'
#' Estimates standard deviation based on a sample. The standard deviation is calculated using the "n-1" method.
#'
#' @param tss Expects an input array whose dimension zero is the length of the time series (all the same) and
#' dimension one indicates the number of time series.
#' @return The sample standard deviation.
#' @export
SampleStdevStatistics <- function(tss) {
  try(out <- .C(
    "sample_stdev_statistics",
    ptr = tss@ptr,
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(tss@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' KurtosisStatistics
#'
#' Returns the kurtosis of tss (calculated with the adjusted Fisher-Pearson standardized moment coefficient G2).
#'
#' @param tss Expects an input array whose dimension zero is the length of the time series (all the same) and
#' dimension one indicates the number of time series.
#' @return The kurtosis of tss.
#' @export
KurtosisStatistics <- function(tss) {
  try(out <- .C(
    "kurtosis_statistics",
    ptr = tss@ptr,
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(tss@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' SkewnessStatistics
#'
#' Calculates the sample skewness of tss (calculated with the adjusted Fisher-Pearson standardized moment
#' coefficient G1).
#'
#' @param tss Expects an input array whose dimension zero is the length of the time series (all the same) and
#' dimension one indicates the number of time series.
#' @return Array containing the skewness of each time series in tss.
#' @export
SkewnessStatistics <- function(tss) {
  try(out <- .C(
    "skewness_statistics",
    ptr = tss@ptr,
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(tss@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' QuantileStatistics
#'
#' Returns values at the given quantile.
#'
#' @param tss Expects an input array whose dimension zero is the length of the time series (all the same) and
#' dimension one indicates the number of time series.
#' @param q Percentile(s) at which to extract score(s). One or many.
#' @param precision Number of decimals expected.
#' @return Values at the given quantile.
#' @export
QuantileStatistics <- function(tss, q, precision = 1e8) {
  try(out <- .C(
    "quantile_statistics",
    ptr = tss@ptr,
    q.ptr = q@ptr,
    as.single(precision),
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(tss@ptr <- out$ptr))
  eval.parent(substitute(q@ptr <- out$q.ptr))
  
  return(createArray(out$b))
}

#' QuantilesCutStatistics
#'
#' Discretizes the time series into equal-sized buckets based on sample quantiles.
#'
#' @param tss Expects an input array whose dimension zero is the length of the time series (all the same) and
#' dimension one indicates the number of time series.
#' @param q Number of quantiles to extract. From 0 to 1, step 1/quantiles.
#' @param precision Number of decimals expected.
#' @return Matrix with the categories, one category per row, the start of the category in the first column and
#' the end in the second category.
#' @export
QuantilesCutStatistics <- function(tss, q, precision = 1e-8) {
  try(out <- .C(
    "quantiles_cut_statistics",
    ptr = tss@ptr,
    as.single(q),
    as.single(precision),
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(tss@ptr <- out$ptr))
  return(createArray(out$b))
}
