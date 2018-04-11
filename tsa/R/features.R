#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

#' AbsEnergy
#' 
#' Calculates de sum over the square values of the time series.
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
    PACKAGE = package
  ))
  
  newList <- list("result" = out$result)
  
  return(newList)
}

#' AbsoluteSumOfChanges
#' 
#' Calculates the sum over the absolute value of consecutive
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
    PACKAGE = package
  ))
  
  newList <- list("result" = out$result)
  
  return(newList)
}

#' AggregatedAutocorrelation
#' 
#' Calculates a linear least-squares regression for values of the time series that were aggregated
#' over chunks versus the sequence from 0 up to the number of chunks minus one.
#'
#' @param tss List of arrays of type double containing the time series.
#' @param aggregation.function Function to be used in the aggregation. It receives an integer which indicates the
#' function to be applied:
#' {
#'   0 : mean,
#'   1 : median
#'   2 : min,
#'   3 : max,
#'   4 : stdev,
#'   5 : var,
#'   default : mean
#' }
#' @return List whose values contains the aggregated correlation for each time series.
#' @export
AggregatedAutocorrelation <- function(tss, aggregation.function) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  try(out <-
        .C(
          "aggregated_autocorrelation",
          tss.concatenated,
          tss.l,
          tss.n,
          as.integer(aggregation.function),
          result = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}

#' AggregatedLinearTrend
#' 
#' Calculates a linear least-squares regression for values of the time series that were aggregated
#' over chunks versus the sequence from 0 up to the number of chunks minus one.
#' 
#' @param tss List of arrays of type double containing the time series.
#' @param chunk.size The chunk size used to aggregate the data.
#' @param aggregation.function Function to be used in the aggregation. It receives an integer which indicates the
#' function to be applied:
#' {
#'   0 : mean,
#'   1 : median
#'   2 : min,
#'   3 : max,
#'   4 : stdev,
#'   default : mean
#' }
#' @return List with:
#' pvalue: The pvalues for all time series.
#' rvalue: The rvalues for all time series.
#' intercept: The intercept values for all time series.
#' slope: The slope for all time series.
#' stderrest: The stderr values for all time series.
#' @export
AggregatedLinearTrend <-
  function(tss, chunk.size, aggregation.function) {
    tss.l <- as.integer64(length(tss[[1]]))
    tss.n <- as.integer64(length(tss))
    tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
    
    try(out <-
          .C(
            "aggregated_linear_trend",
            tss.concatenated,
            tss.l,
            tss.n,
            as.integer64(chunk.size),
            as.integer(aggregation.function),
            slope = as.double(seq(
              length = (tss.n),
              from = 0,
              to = 0
            )),
            intercept = as.double(seq(
              length = (tss.n),
              from = 0,
              to = 0
            )),
            rvalue = as.double(seq(
              length = (tss.n),
              from = 0,
              to = 0
            )),
            pvalue = as.double(seq(
              length = (tss.n),
              from = 0,
              to = 0
            )),
            stderrest = as.double(seq(
              length = (tss.n),
              from = 0,
              to = 0
            )),
            PACKAGE = package
          ))
    result <-
      list(
        "slope" = out$slope,
        "intercept" = out$intercept,
        "rvalue" = out$rvalue,
        "pvalue" = out$pvalue,
        "stderrest" = out$stderrest
      )
    return(result)
  }

#' ApproximateEntropy
#'
#' Calculates a vectorized Approximate entropy algorithm.
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
    PACKAGE = package
  ))
  r.result <- list("result" = out$result)
  
  return(r.result)
}

#' CrossCovariance
#' 
#' Calculates the cross-covariance of the given time series
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
    PACKAGE = package
  ))
  r.result <- list("result" = out$result)
  
  return(r.result)
}

#' AutoCovariance
#' 
#' Calculates the auto-covariance the given time series.
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
    PACKAGE = package
  ))
  r.result <- list("result" = out$result)
  
  return(r.result)
}

#' CrossCorrelation
#'
#' Calculates the cross-correlation of the given time series.
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
    PACKAGE = package
  ))
  r.result <- list("result" = out$result)
  
  return(r.result)
}

#' AutoCorrelation
#' 
#' Calculates the autocorrelation of the specified lag for the given time series.
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
    PACKAGE = package
  ))
  
  return(out$result)
}

#' BinnedEntropy
#' 
#' Calculates the binned entropy for the given time series and number of bins.
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
    PACKAGE = package
  ))
  
  return(out$result)
}

#' C3
#'
#' Calculates the Schreiber, T. and Schmitz, A. (1997) measure of non-linearity
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
    PACKAGE = package
  ))
  
  r.result <- list("result" = out$result)
  
  return(r.result)
}

#' CicCe
#' 
#' Calculates an estimate for the time series complexity defined by
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
    PACKAGE = package
  ))
  
  r.result <- list("result" = out$result)
  
  return(r.result)
}

#' CountAboveMean
#' 
#' Calculates the number of values in the time series that are higher than
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
    PACKAGE = package
  ))
  
  return(out$result)
}

#' CountBelowMean
#'
#' Calculates the number of values in the time series that are lower than
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
    PACKAGE = package
  ))
  
  return(out$result)
}

#' CwtCoefficients
#' Calculates a Continuous wavelet transform for the Ricker wavelet, also known as
#' the "Mexican hat wavelet".
#' 
#' @param tss List of arrays of type double containing the time series.
#' @param widths Widths. List of arrays of type double containing the time series.
#' @param coeff Coefficient of interest.
#' @param w Width of interest.
#' @return Result of calculated coefficients.
#' @export
CwtCoefficients <- function(tss, widths, coeff, w) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  widths.l <- as.integer64(length(widths[[1]]))
  widths.n <- as.integer64(length(widths))
  widths.concatenated <- as.integer(apply(cbind(widths), 1, unlist))
  
  try(out <-
        .C(
          "cwt_coefficients",
          tss.concatenated,
          tss.l,
          tss.n,
          widths.concatenated,
          widths.l,
          widths.n,
          as.integer(coeff),
          as.integer(w),
          result = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}

#' EnergyRatioByChunks
#' 
#' Calculates the sum of squares of chunk i out of N chunks expressed as a ratio
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
    PACKAGE = package
  ))
  
  return(out$result)
}

#' FftAggregated
#'
#' Calculates the spectral centroid(mean), variance, skew, and kurtosis of the absolute fourier transform
#' spectrum.
#'
#' @param tss List of arrays of type double containing the time series.
#' @return The spectral centroid (mean), variance, skew, and kurtosis of the absolute fourier transform
#'  spectrum.
#' @export
FftAggregated <- function(tss) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  try(out <-
        .C(
          "fft_aggregated",
          tss.concatenated,
          tss.l,
          tss.n,
          result = as.double(seq(
            length = (tss.n * tss.l),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}

#' FftCoefficient
#' 
#' Calculates the fourier coefficients of the one-dimensional discrete
#' Fourier Transform for real input by fast fourier transformation algorithm.
#' 
#' @param tss List of arrays of type double containing the time series.
#' @return List with:
#' real: The real part of the coefficient.
#' imag: The imaginary part of the coefficient.
#' abs: The absolute value of the coefficient.
#' angle: The angle of the coefficient.
#' @export
FftCoefficient <- function(tss, coefficient) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  try(out <-
        .C(
          "fftCoefficient",
          tss.concatenated,
          tss.l,
          tss.n,
          as.integer64(coefficient),
          real = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          imag = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          abs = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          angle = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  result <-
    (list(
      "real" = out$real,
      "imag" = out$imag,
      "abs" = out$abs,
      "angle" = out$angle
    ))
  return(result)
}

#' FirstLocationOfMaximum
#' 
#' Calculates the first relative location of the maximal value for each timeseries.
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
    PACKAGE = package
  ))
  
  return(out$result)
}

#' FistLocationOfMinimum
#' 
#' Calculates the first location of the minimal value of each time series. The position
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
    PACKAGE = package
  ))
  
  return(out$result)
}

#' HasDuplicates
#' 
#' Calculates if the input time series contain duplicated elements.
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
    PACKAGE = package
  ))
  
  return(out$result)
}

#' HasDuplicateMax
#' 
#' Calculates if the maximum within input time series is duplicated.
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
    PACKAGE = package
  ))
  
  return(out$result)
}

#' HasDuplicateMin
#' 
#' Calculates if the minimum of the input time series is duplicated.
#'
#' @param tss List of arrays of type double containing the time series.
#' @return List with an array containing True if the minimum of the time series is duplicated
#' and False otherwise.
#' @export
HasDuplicateMin <- function(tss) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  try(out <-
        .C(
          "has_duplicate_min",
          tss.concatenated,
          tss.l,
          tss.n,
          result = as.logical(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  
  return(out$result)
  
}

#' IndexMaxQuantile
#' 
#' Calculates the index of the max quantile.
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
    PACKAGE = package
  ))
  
  return(out$result)
}

#' Kurtosis
#' 
#' Returns the kurtosis of tss (calculated with the adjusted Fisher-Pearson
#' standardized moment coefficient G2).
#'
#' @param tss List of arrays of type double containing the time series.
#' @return The kurtosis of each tss.
#' @export
Kurtosis <- function(tss) {
  tss.length <- as.integer64(length(tss[[1]]))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  tss.number.of.ts <- as.integer64(length(tss))
  
  try(out <- .C(
    "kurtosis",
    tss.concatenated,
    tss.length,
    tss.number.of.ts,
    result = as.double(seq(
      length = (tss.number.of.ts),
      from = 0,
      to = 0
    )),
    PACKAGE = package
  ))
  
  return(out$result)
}

#' LargeStandardDeviation
#' 
#' Checks if the time series within tss have a large standard deviation.
#'
#' @param tss List of arrays of type double containing the time series.
#' @param r The threshold.
#' @return Array containing True for those time series in tss that have a large standard deviation.
#' @export
LargeStandardDeviation <- function(tss, r) {
  tss.length <- as.integer64(length(tss[[1]]))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  tss.number.of.ts <- as.integer64(length(tss))
  
  try(out <- .C(
    "large_standard_deviation",
    tss.concatenated,
    tss.length,
    tss.number.of.ts,
    as.double(r),
    result = as.logical(seq(
      length = (tss.number.of.ts),
      from = 0,
      to = 0
    )),
    PACKAGE = package
  ))
  
  return(out$result)
}

#' LastLocationOfMaximum
#' 
#' Calculates the last location of the maximum value of each time series. The position
#' is calculated relatively to the length of the series.
#'
#' @param tss List of arrays of type double containing the time series.
#' @return The last relative location of the maximum value of each series.
#' @export
LastLocationOfMaximum <- function(tss) {
  tss.length <- as.integer64(length(tss[[1]]))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  tss.number.of.ts <- as.integer64(length(tss))
  
  try(out <- .C(
    "last_location_of_maximum",
    tss.concatenated,
    tss.length,
    tss.number.of.ts,
    result = as.double(seq(
      length = (tss.number.of.ts),
      from = 0,
      to = 0
    )),
    PACKAGE = package
  ))
  
  return(out$result)
}

#' LastLocationOfMinimum
#' 
#' Calculates the last location of the minimum value of each time series. The position
#' is calculated relatively to the length of the series.
#'
#' @param tss List of arrays of type double containing the time series.
#' @return The last relative location of the minimum value of each series.
#' @export
LastLocationOfMinimum <- function(tss) {
  tss.length <- as.integer64(length(tss[[1]]))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  tss.number.of.ts <- as.integer64(length(tss))
  
  try(out <- .C(
    "last_location_of_minimum",
    tss.concatenated,
    tss.length,
    tss.number.of.ts,
    result = as.double(seq(
      length = (tss.number.of.ts),
      from = 0,
      to = 0
    )),
    PACKAGE = package
  ))
  
  return(out$result)
}

#' Length
#' 
#' Returns the length of the input time series.
#'
#' @param tss List of arrays of type double containing the time series.
#' @return The length of tss.
#' @export
Length <- function(tss) {
  tss.length <- as.integer64(length(tss[[1]]))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  tss.number.of.ts <- as.integer64(length(tss))
  
  try(out <- .C(
    "length",
    tss.concatenated,
    tss.length,
    tss.number.of.ts,
    result = as.integer(seq(
      length = (tss.number.of.ts),
      from = 0,
      to = 0
    )),
    PACKAGE = package
  ))
  
  return(out$result)
}

#' LinearTrend
#' 
#' Calculates a linear least-squares regression for the values of the time series versus the sequence from 0 to
#' length of the time series minus one.
#'
#' @param tss List of arrays of type double containing the time series.
#' @return a list with:
#' pvalue: The pvalues for all time series.
#' rvalue: The rvalues for all time series.
#' intercept: The intercept values for all time series.
#' slope: The slope for all time series.
#' stdrr: The stderr values for all time series.
#' @export
LinearTrend <- function(tss) {
  tss.length <- as.integer64(length(tss[[1]]))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  tss.number.of.ts <- as.integer64(length(tss))
  
  try(out <- .C(
    "linear_trend",
    tss.concatenated,
    tss.length,
    tss.number.of.ts,
    pvalue = as.double(seq(
      length = (tss.number.of.ts),
      from = 0,
      to = 0
    )),
    rvalue = as.double(seq(
      length = (tss.number.of.ts),
      from = 0,
      to = 0
    )),
    intercept = as.double(seq(
      length = (tss.number.of.ts),
      from = 0,
      to = 0
    )),
    slope = as.double(seq(
      length = (tss.number.of.ts),
      from = 0,
      to = 0
    )),
    stdrr = as.double(seq(
      length = (tss.number.of.ts),
      from = 0,
      to = 0
    )),
    PACKAGE = package
  ))
  
  result <- list(
    "pvalue" = out$pvalue,
    "rvalue" = out$rvalue,
    "intercept" = out$intercept,
    "slope" = out$slope,
    "stdrr" = out$stdrr
  )
  return(result)
}

#' LongestStrikeAboveMean
#' 
#' Calculates the length of the longest consecutive subsequence in tss that is bigger than the mean of tss.
#'
#' @param tss List of arrays of type double containing the time series.
#' @return The length of the longest consecutive subsequence in the input time series that is bigger than the mean.
#' @export
LongestStrikeAboveMean <- function(tss) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  
  try(out <-
        .C(
          "longest_strike_above_mean",
          tss.concatenated,
          tss.l,
          tss.n,
          result = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}

#' LongestStrikeBelowMean
#' 
#' Calculates the length of the longest consecutive subsequence in tss that is below the mean of tss.
#'
#' @param tss List of arrays of type double containing the time series.
#' @return The length of the longest consecutive subsequence in the input time series that is below the mean.
#' @export
LongestStrikeBelowMean <- function(tss) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  try(out <-
        .C(
          "longest_strike_below_mean",
          tss.concatenated,
          tss.l,
          tss.n,
          result = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}

#' MaxLangevinFixedPoint
#' 
#' Largest fixed point of dynamics \eqn{\max_x {h(x)=0}} estimated from polynomial
#' \eqn{h(x)}, which has been fitted to the deterministic dynamics of Langevin model
#' \deqn{
#'   \dot(x)(t) = h(x(t)) + R \mathcal(N)(0,1)
#'   }
#' as described by
#' Friedrich et al. (2000): Physics Letters A 271, p. 217-222 *Extracting model equations from experimental data.
#
#'
#' @param tss List of arrays of type double containing the time series.
#' @param m: Order of polynom to fit for estimating fixed points of dynamics.
#' @param r: Number of quantiles to use for averaging.
#' @return Largest fixed point of deterministic dynamics.
#' @export
MaxLangevinFixedPoint <- function(tss, m, r) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  try(out <-
        .C(
          "max_langevin_fixed_point",
          tss.concatenated,
          tss.l,
          tss.n,
          as.integer(m),
          as.double(r),
          result = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}

#' Mean
#' 
#' Calculates the mean value for each time series within tss.
#'
#' @param tss List of arrays of type double containing the time series.
#' @return The mean value of each time series within tss.
#' @export
Mean <- function(tss) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  try(out <-
        .C(
          "mean",
          tss.concatenated,
          tss.l,
          tss.n,
          result = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}

#' Maximum
#' 
#' Calculates the maximum value for each time series within tss.
#'
#' @param tss List of arrays of type double containing the time series.
#' @return The maximum value of each time series within tss.
#' @export
Maximum <- function(tss) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  try(out <-
        .C(
          "maximum",
          tss.concatenated,
          tss.l,
          tss.n,
          result = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}

#' MeanAbsolutgeChange
#' 
#' Calculates the mean over the absolute differences between subsequent time series values in tss.
#'
#' @param tss List of arrays of type double containing the time series.
#' @return The mean over the absolute differences between subsequent time series values.
#' @export
MeanAbsoluteChange <- function(tss) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  try(out <-
        .C(
          "mean_absolute_change",
          tss.concatenated,
          tss.l,
          tss.n,
          result = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}

#' MeanChange
#' 
#' Calculates the mean over the differences between subsequent time series values in tss.
#'
#' @param tss List of arrays of type double containing the time series.
#' @return The mean over the differences between subsequent time series values.
#' @export
MeanChange <- function(tss) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  
  try(out <-
        .C(
          "mean_change",
          tss.concatenated,
          tss.l,
          tss.n,
          result = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}

#' MeanSecondDerivativeCentral
#' 
#' Calculates mean value of a central approximation of the second derivative for each time series in tss.
#'
#' @param tss List of arrays of type double containing the time series.
#' @return The mean value of a central approximation of
#' the second derivative for each time series.
#' @export
MeanSecondDerivativeCentral <- function(tss) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  try(out <-
        .C(
          "mean_second_derivative_central",
          tss.concatenated,
          tss.l,
          tss.n,
          result = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}

#' Median
#' 
#' Calculates the median value for each time series within tss.
#' 
#' @param tss List of arrays of type double containing the time series.
#' @return The median value of each time series within tss.
#' @export
Median <- function(tss) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  try(out <-
        .C(
          "median",
          tss.concatenated,
          tss.l,
          tss.n,
          result = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}

#' Minimum
#'
#' Calculates the minimum value for each time series within tss.
#' 
#' @param tss List of arrays of type double containing the time series.
#' @return The minimum value of each time series within tss.
#' @export
Minimum <- function(tss) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  try(out <-
        .C(
          "minimum",
          tss.concatenated,
          tss.l,
          tss.n,
          result = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}

#' NumberCrossingM
#' 
#' Calculates the number of m-crossings. A m-crossing is defined as two sequential values where the first
#' value is lower than m and the next is greater, or viceversa. If you set m to zero, you will get the number of
#' zero crossings.
#' 
#' @param tss List of arrays of type double containing the time series.
#' @param m The m value.
#' @return The number of m-crossings of each time series within tss.
#' @export
NumberCrossingM <- function(tss, m) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  try(out <-
        .C(
          "number_crossing_m",
          tss.concatenated,
          tss.l,
          tss.n,
          as.integer(m),
          result = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}

#' NumberPeaks
#' 
#' Calculates the number of peaks of at least support \eqn{n} in the time series \eqn{tss}. A peak of support
#' \eqn{n} is defined as a subsequence of \eqn{tss} where a value occurs, which is bigger than
#' its \eqn{n} neighbours to the left and to the right.
#' 
#' @param tss List of arrays of type double containing the time series.
#' @param n The support of the peak.
#' @return The number of m-crossings of each time series within tss.
#' @export
NumberPeaks <- function(tss, n) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  try(out <-
        .C(
          "number_peaks",
          tss.concatenated,
          tss.l,
          tss.n,
          as.integer(n),
          result = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}

#' PercentageOfReoccurringDatapointsToAllDatapoints
#' 
#' Calculates the number of peaks of at least support \eqn{n} in the time series \eqn{tss}. A peak of support
#' \eqn{n} is defined as a subsequence of \eqn{tss} where a value occurs, which is bigger than
#' its \eqn{n} neighbours to the left and to the right.
#' 
#' @param tss List of arrays of type double containing the time series.
#' @param n The support of the peak.
#' @return The number of m-crossings of each time series within tss.
#' @export
PercentageOfReoccurringDatapointsToAllDatapoints <-
  function(tss, isSorted) {
    tss.l <- as.integer64(length(tss[[1]]))
    tss.n <- as.integer64(length(tss))
    tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
    
    try(out <-
          .C(
            "percentage_of_reoccurring_datapoints_to_all_datapoints",
            tss.concatenated,
            tss.l,
            tss.n,
            as.logical(isSorted),
            result = as.double(seq(
              length = (tss.n),
              from = 0,
              to = 0
            )),
            PACKAGE = package
          ))
    return(out$result)
  }

#' Quantile
#' 
#' Returns values at the given quantile.
#' 
#' @param tss List of arrays of type double containing the time series.
#' @param q Percentile(s) at which to extract score(s). One or many.
#' @param precision Number of decimals expected.
#' @return The number of m-crossings of each time series within tss.
#' @export
Quantile <- function(tss, q, precision = 1e8) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  q.l <- as.integer64(length(q))
  q <- as.double(q)
  try(out <-
        .C(
          "quantile",
          tss.concatenated,
          tss.l,
          tss.n,
          q,
          q.l,
          as.double(precision),
          result = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}

#' RatioBeyondRSigma
#' 
#' Calculates the ratio of values that are more than  \eqn{r*std(x)} (so \eqn{r} sigma away from the mean of
#' \eqn{x}.
#' 
#' @param tss List of arrays of type double containing the time series.
#' @param q Percentile(s) at which to extract score(s). One or many.
#' @param precision Number of decimals expected.
#' @return The number of m-crossings of each time series within tss.
#' @export
RatioBeyondRSigma <- function(tss, r) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  try(out <-
        .C(
          "ratio_beyond_r_sigma",
          tss.concatenated,
          tss.l,
          tss.n,
          as.double(r),
          result = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}

#' SampleEntropy
#' 
#' Calculates a vectorized sample entropy algorithm.
#' https://en.wikipedia.org/wiki/Sample_entropy
#' https://www.ncbi.nlm.nih.gov/pubmed/10843903?dopt=Abstract
#' For short time-series this method is highly dependent on the parameters, but should be stable for N > 2000,
#' see: Yentes et al. (2012) - The Appropriate Use of Approximate Entropy and Sample Entropy with Short Data Sets
#' Other shortcomings and alternatives discussed in:
#' Richman & Moorman (2000) - Physiological time-series analysis using approximate entropy and sample entropy.
#' 
#' @param tss List of arrays of type double containing the time series.
#' @return An array with the same dimensions as tss, whose values (time series in dimension 0)
#' contains the vectorized sample entropy for all the input time series in tss.
#' @export
SampleEntropy <- function(tss) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  try(out <-
        .C(
          "sample_entropy",
          tss.concatenated,
          tss.l,
          tss.n,
          result = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}

#' Skewness
#' 
#' Calculates the sample skewness of tss (calculated with the adjusted Fisher-Pearson standardized
#' moment coefficient G1).
#' 
#' @param tss List of arrays of type double containing the time series.
#' @return Array containing the skewness of each time series in tss.
#' @export
Skewness <- function(tss) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  try(out <-
        .C(
          "skewness",
          tss.concatenated,
          tss.l,
          tss.n,
          result = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}

#' Skewness
#' 
#' Calculates the standard deviation of each time series within tss.
#' 
#' @param tss List of arrays of type double containing the time series.
#' @return The standard deviation of each time series within tss.
#' @export
StandardDeviation <- function(tss) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  try(out <-
        .C(
          "standard_deviation",
          tss.concatenated,
          tss.l,
          tss.n,
          result = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}

#' SumOfReoccuringDatapoints
#' 
#' Calculates the sum of all data points, that are present in the time series more than once.
#' 
#' @param tss List of arrays of type double containing the time series.
#' @return Returns the sum of all data points, that are present in the time series more than once.
#' @export
SumOfReoccurringDatapoints <- function(tss, is.sorted = FALSE) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  try(out <-
        .C(
          "sum_of_reoccurring_datapoints",
          tss.concatenated,
          tss.l,
          tss.n,
          as.logical(is.sorted),
          result = as.double(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}

#' SymmetryLooking
#' 
#' Calculates if the distribution of tss *looks symmetric*. This is the case if
#' \deqn{
#'  | mean(tss)-median(tss)| < r * (max(tss)-min(tss))
#' }
#' 
#' @param tss List of arrays of type double containing the time series.
#' @param r The percentage of the range to compare with.
#' @return An array denoting if the input time series look symmetric.
#' @export
SymmetryLooking <- function(tss, r) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  
  try(out <-
        .C(
          "symmetry_looking",
          tss.concatenated,
          tss.l,
          tss.n,
          as.double(r),
          result = as.logical(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}

#' ValueCount
#' 
#' Counts occurrences of value in the time series tss.
#' 
#' @param tss List of arrays of type double containing the time series.
#' @param v The value to be counted.
#' @return An array containing the count of the given value in each time series.
#' @export
ValueCount <- function(tss, v) {
  tss.l <- as.integer64(length(tss[[1]]))
  tss.n <- as.integer64(length(tss))
  tss.concatenated <- as.integer(apply(cbind(tss), 1, unlist))
  
  try(out <-
        .C(
          "value_count",
          tss.concatenated,
          tss.l,
          tss.n,
          as.double(v),
          result = as.integer(seq(
            length = (tss.n),
            from = 0,
            to = 0
          )),
          PACKAGE = package
        ))
  return(out$result)
}