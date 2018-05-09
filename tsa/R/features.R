#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

#' AbsEnergy
#'
#' Calculates the sum over the square values of the time series.
#'
#' @param arr TSA array with the time series.
#' @return TSA array with the Absolute Energy.
#' @export
AbsEnergy <- function(arr) {
  try(out <- .C("abs_energy",
                ptr = arr@ptr,
                b = as.integer64(0),
                PACKAGE = package))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' AbsoluteSumOfChanges
#'
#' Calculates the sum over the absolute value of consecutive
#' changes in the time series.
#'
#' @param arr TSA array with the time series.
#' @return TSA Array with the AbsoluteSumOfChanges
#' @export
AbsoluteSumOfChanges <- function(arr) {
  try(out <- .C(
    "absolute_sum_of_changes",
    ptr = arr@ptr,
    b = as.integer64(0),
    PACKAGE = package
  ))
  
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' AggregatedAutocorrelation
#'
#' Calculates a linear least-squares regression for values of the time series that were aggregated
#' over chunks versus the sequence from 0 up to the number of chunks minus one.
#'
#' @param arr TSA array with the time series.
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
#' @return TSA Array whose values contains the aggregated correlation for each time series.
#' @export
AggregatedAutocorrelation <- function(arr, aggregation.function) {
  try(out <-
        .C(
          "aggregated_autocorrelation",
          ptr = arr@ptr,
          as.integer(aggregation.function),
          b = as.integer64(0),
          PACKAGE = package
        ))
  
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' AggregatedLinearTrend
#'
#' Calculates a linear least-squares regression for values of the time series that were aggregated
#' over chunks versus the sequence from 0 up to the number of chunks minus one.
#'
#' @param arr TSA array with the time series.
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
#' @return List of TSA Arrays with:
#' {
#'     pvalue: The pvalues for all time series.
#'     rvalue: The rvalues for all time series.
#'     intercept: The intercept values for all time series.
#'     slope: The slope for all time series.
#'     stderrest: The stderr values for all time series.
#' }
#' @export
AggregatedLinearTrend <-
  function(arr, chunk.size, aggregation.function) {
    try(out <-
          .C(
            "aggregated_linear_trend",
            ptr = arr@ptr,
            as.integer64(chunk.size),
            as.integer(aggregation.function),
            slope = as.integer64(0),
            intercept = as.integer64(0),
            rvalue = as.integer64(0),
            pvalue = as.integer64(0),
            stderrest = as.integer64(0),
            PACKAGE = package
          ))
    result <-
      list(
        "slope" = createArray(out$slope),
        "intercept" = createArray(out$intercept),
        "rvalue" = createArray(out$rvalue),
        "pvalue" = createArray(out$pvalue),
        "stderrest" = createArray(out$stderrest)
      )
    eval.parent(substitute(arr@ptr <- out$ptr))
    return(result)
  }

#' ApproximateEntropy
#'
#' Calculates a vectorized Approximate entropy algorithm.
#' https://en.wikipedia.org/wiki/Approximate_entropy
#' For short time-series this method is highly dependent on the parameters, but should be stable for N > 2000,
#' see: Yentes et al. (2012) - The Appropriate Use of Approximate Entropy and Sample Entropy with Short Data Sets
#' Other shortcomings and alternatives discussed in:
#' Richman & Moorman (2000) - Physiological time-series analysis using approximate entropy and sample entropy.
#'
#'
#' @param arr TSA array with the time series.
#' @param m Length of compared run of data.
#' @param r Filtering level, must be positive.
#' @return The vectorized approximate entropy for all the input time series in arr.
#' @export
ApproximateEntropy <- function(arr, m, r) {
  try(out <- .C(
    "approximate_entropy",
    ptr = arr@ptr,
    as.integer(m),
    as.single(r),
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' CrossCovariance
#'
#' Calculates the cross-covariance of the given time series.
#'
#' @param xss.arr TSA array with the time series.
#' @param yss.arr TSA array with the time series.
#' @param unbiased Determines whether it divides by n - lag (if true) or n (if false).
#' @return The cross-covariance value for the given time series.
#' @export
CrossCovariance <- function(arr.xss, arr.yss, unbiased) {
  try(out <- .C(
    "cross_covariance",
    xss.ptr = arr.xss@ptr,
    yss.ptr = arr.yss@ptr,
    unbiased,
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(arr.xss@ptr <- out$xss.ptr))
  eval.parent(substitute(arr.yss@ptr <- out$yss.ptr))
  
  return(createArray(out$b))
}

#' AutoCovariance
#'
#' Calculates the auto-covariance the given time series.
#'
#' @param arr TSA array with the time series.
#' @param unbiased Determines whether it divides by n - lag (if true) or n (if false).
#' @return The auto-covariance value for the given time series.
#' @export
AutoCovariance <- function(arr, unbiased) {
  try(out <- .C(
    "auto_covariance",
    ptr = arr@ptr,
    unbiased,
    b = as.integer64(0),
    PACKAGE = package
  ))
  r.result <- list("result" = out$result)
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' CrossCorrelation
#'
#' Calculates the cross-correlation of the given time series.
#'
#' @param xss.arr TSA array with the time series.
#' @param yss.arr TSA array with the time series.
#' @param unbiased Determines whether it divides by n - lag (if true) or n (if false).
#' @return The cross-correlation value for the given time series.
#' @export
CrossCorrelation <- function(arr.xss, arr.yss, unbiased) {
  try(out <- .C(
    "cross_correlation",
    xss.ptr = arr.xss@ptr,
    yss.ptr = arr.yss@ptr,
    unbiased,
    b = as.integer64(0),
    PACKAGE = package
  ))
  
  eval.parent(substitute(arr.xss@ptr <- out$xss.ptr))
  eval.parent(substitute(arr.yss@ptr <- out$yss.ptr))
  
  return(createArray(out$b))
}

#' AutoCorrelation
#'
#' Calculates the autocorrelation of the specified lag for the given time series.
#'
#' @param arr TSA array with the time series.
#' @param max_lag The maximum lag to compute.
#' @param unbiased Determines whether it divides by n - lag (if true) or n (if false).
#' @return The autocorrelation value for the given time series.
#' @export
AutoCorrelation <- function(arr, max.lag, unbiased) {
  try(out <- .C(
    "auto_correlation",
    ptr = arr@ptr,
    as.integer64(max.lag),
    unbiased,
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' BinnedEntropy
#'
#' Calculates the binned entropy for the given time series and number of bins.
#'
#' @param arr TSA array with the time series.
#' @param max.bins The number of bins.
#' @return The binned entropy value for the given time series.
#' @export
BinnedEntropy <- function(arr, max.bins) {
  try(out <- .C(
    "binned_entropy",
    ptr = arr@ptr,
    as.integer(max.bins),
    b = as.integer64(0),
    PACKAGE = package
  ))
  
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' C3
#'
#' Calculates the Schreiber, T. and Schmitz, A. (1997) measure of non-linearity
#' for the given time series.
#'
#' @param arr TSA array with the time series.
#' @param lag The lag.
#' @return The non-linearity value for the given time series.
#' @export
C3 <- function(arr, lag) {
  try(out <- .C(
    "c3",
    ptr = arr@ptr,
    as.integer64(lag),
    b = as.integer64(0),
    PACKAGE = package
  ))
  
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' CicCe
#'
#' Calculates an estimate for the time series complexity defined by
#' Batista, Gustavo EAPA, et al (2014). (A more complex time series has more peaks,
#' valleys, etc.)
#'
#' @param arr TSA array with the time series.
#' @param z.normalize Controls whether the time series should be z-normalized or not.
#' @return The complexity value for the given time series.
#' @export
CidCe <- function(arr, z.normalize) {
  try(out <- .C(
    "cid_ce",
    ptr = arr@ptr,
    z.normalize,
    b = as.integer64(0),
    PACKAGE = package
  ))
  
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' CountAboveMean
#'
#' Calculates the number of values in the time series that are higher than
#' the mean.
#'
#' @param arr TSA array with the time series.
#' @return The number of values in the time series that are higher than the mean.
#' @export
CountAboveMean <- function(arr) {
  try(out <- .C(
    "count_above_mean",
    ptr = arr@ptr,
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' CountBelowMean
#'
#' Calculates the number of values in the time series that are lower than
#' the mean.
#'
#' @param arr TSA array with the time series.
#' @return The number of values in the time series that are lower than the mean.
#' @export
CountBelowMean <- function(arr) {
  try(out <- .C(
    "count_below_mean",
    ptr = arr@ptr,
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' CwtCoefficients
#'
#' Calculates a Continuous wavelet transform for the Ricker wavelet, also known as
#' the "Mexican hat wavelet".
#'
#' @param arr TSA array with the time series.
#' @param arr.widths Widths. TSA array.
#' @param coeff Coefficient of interest.
#' @param w Width of interest.
#' @return Result of calculated coefficients.
#' @export
CwtCoefficients <- function(arr, arr.widths, coeff, w) {
  try(out <-
        .C(
          "cwt_coefficients",
          arr.ptr = arr@ptr,
          widths.ptr = arr.widths@ptr,
          as.integer(coeff),
          as.integer(w),
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$arr.ptr))
  eval.parent(substitute(arr.widths@ptr <- out$widths.ptr))
  
  return(createArray(out$b))
}

#' EnergyRatioByChunks
#'
#' Calculates the sum of squares of chunk i out of N chunks expressed as a ratio
#' with the sum of squares over the whole series. segmentFocus should be lower
#' than the number of segments.
#'
#' @param arr TSA array with the time series.
#' @param num.segments The number of segments to divide the series into.
#' @param segment.focus The segment number (starting at zero) to return a feature on.
#' @return The energy ratio by chunk of the time series.
#' @export
EnergyRatioByChunks <- function(arr, num.segments, segment.focus) {
  try(out <- .C(
    "energy_ratio_by_chunks",
    ptr = arr@ptr,
    as.integer64(num.segments),
    as.integer64(segment.focus),
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' FftAggregated
#'
#' Calculates the spectral centroid(mean), variance, skew, and kurtosis of the absolute fourier transform
#' spectrum.
#'
#' @param arr TSA array with the time series.
#' @return The spectral centroid (mean), variance, skew, and kurtosis of the absolute fourier transform
#'  spectrum.
#' @export
FftAggregated <- function(arr) {
  try(out <-
        .C(
          "fft_aggregated",
          ptr = arr@ptr,
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' FftCoefficient
#'
#' Calculates the fourier coefficients of the one-dimensional discrete
#' Fourier Transform for real input by fast fourier transformation algorithm.
#'
#' @param arr TSA array with the time series.
#' @return TSA Array with:
#' real: The real part of the coefficient.
#' imag: The imaginary part of the coefficient.
#' abs: The absolute value of the coefficient.
#' angle: The angle of the coefficient.
#' @export
FftCoefficient <- function(arr, coefficient) {
  try(out <-
        .C(
          "fft_coefficient",
          ptr = arr@ptr,
          as.integer64(coefficient),
          real = as.integer64(0),
          imag = as.integer64(0),
          abs = as.integer64(0),
          angle = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  result <-
    (
      list(
        "real" = createArray(out$real),
        "imag" = createArray(out$imag),
        "abs" = createArray(out$abs),
        "angle" = createArray(out$angle)
      )
    )
  return(result)
}

#' FirstLocationOfMaximum
#'
#' Calculates the first relative location of the maximal value for each timeseries.
#'
#' @param arr TSA array with the time series.
#' @return The first relative location of the maximum value to the length of the timeseries,
#' for each timeseries.
#' @export
#'
FirstLocationOfMaximum <- function(arr) {
  try(out <- .C(
    "first_location_of_maximum",
    ptr = arr@ptr,
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' FistLocationOfMinimum
#'
#' Calculates the first location of the minimal value of each time series. The position
#' is calculated relatively to the length of the series.
#'
#' @param arr TSA array with the time series.
#' @return The first relative location of the minimal value of each series.
#' @export
#'
FirstLocationOfMinimum <- function(arr) {
  try(out <- .C(
    "first_location_of_minimum",
    ptr = arr@ptr,
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#'  FriedrichCoefficients
#'
#'  Coefficients of polynomial \eqn{h(x)}, which has been fitted to the deterministic dynamics of Langevin model:
#'  Largest fixed point of dynamics  \eqn{argmax_x {h(x)=0}} estimated from polynomial \eqn{h(x)`},
#'  which has been fitted to the deterministic dynamics of Langevin model:
#'  \deqn{
#'       \dot(x)(t) = h(x(t)) + R \mathcal(N)(0,1)
#'  }
#' as described by [1]. For short time series this method is highly dependent on the parameters.
#'
#' [1] Friedrich et al. (2000): Physics Letters A 271, p. 217-222
#' Extracting model equations from experimental data.
#'
#' @param arr: TSA array with the time series.
#' @param m: Order of polynom to fit for estimating fixed points of dynamics.
#' @param r: Number of quantiles to use for averaging.
#' @return: TSA array with the coefficients for each time series.
#' @export
FriedrichCoefficients <- function(arr, m, r) {
  try(out <- .C(
    "friedrich_coefficients",
    ptr = arr@ptr,
    as.integer(m),
    as.single(r),
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' HasDuplicates
#'
#' Calculates if the input time series contain duplicated elements.
#'
#' @param arr TSA array with the time series.
#' @return Array containing True if the time series contains duplicated elements
#' and false otherwise.
#' @export
#'
HasDuplicates <- function(arr) {
  try(out <- .C(
    "has_duplicates",
    ptr = arr@ptr,
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' HasDuplicateMax
#'
#' Calculates if the maximum within input time series is duplicated.
#'
#' @param arr TSA array with the time series.
#' @return Array containing True if the maximum value of the time series is duplicated
#' and false otherwise.
#' @export
#'
HasDuplicateMax <- function(arr) {
  try(out <- .C(
    "has_duplicate_max",
    ptr = arr@ptr,
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' HasDuplicateMin
#'
#' Calculates if the minimum of the input time series is duplicated.
#'
#' @param arr TSA array with the time series.
#' @return TSA Array with an array containing True if the minimum of the time series is duplicated
#' and False otherwise.
#' @export
HasDuplicateMin <- function(arr) {
  try(out <-
        .C(
          "has_duplicate_min",
          ptr = arr@ptr,
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
  
}

#' IndexMassQuantile
#'
#' Calculates the index of the mass quantile.
#'
#' @param arr TSA array with the time series.
#' @param q The quantile.
#' @return The index of the mass quantile q.
#' @export
#'
IndexMassQuantile <- function(arr, q) {
  try(out <- .C(
    "index_mass_quantile",
    ptr = arr@ptr,
    as.single(q),
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' Kurtosis
#'
#' Returns the kurtosis of arr (calculated with the adjusted Fisher-Pearson
#' standardized moment coefficient G2).
#'
#' @param arr TSA array with the time series.
#' @return The kurtosis of each arr.
#' @export
Kurtosis <- function(arr) {
  try(out <- .C("kurtosis",
                ptr = arr@ptr,
                b = as.integer64(0),
                PACKAGE = package))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' LargeStandardDeviation
#'
#' Checks if the time series within arr have a large standard deviation.
#'
#' @param arr TSA array with the time series.
#' @param r The threshold.
#' @return Array containing True for those time series in arr that have a large standard deviation.
#' @export
LargeStandardDeviation <- function(arr, r) {
  try(out <- .C(
    "large_standard_deviation",
    ptr = arr@ptr,
    as.single(r),
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' LastLocationOfMaximum
#'
#' Calculates the last location of the maximum value of each time series. The position
#' is calculated relatively to the length of the series.
#'
#' @param arr TSA array with the time series.
#' @return The last relative location of the maximum value of each series.
#' @export
LastLocationOfMaximum <- function(arr) {
  try(out <- .C(
    "last_location_of_maximum",
    ptr = arr@ptr,
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' LastLocationOfMinimum
#'
#' Calculates the last location of the minimum value of each time series. The position
#' is calculated relatively to the length of the series.
#'
#' @param arr TSA array with the time series.
#' @return The last relative location of the minimum value of each series.
#' @export
LastLocationOfMinimum <- function(arr) {
  try(out <- .C(
    "last_location_of_minimum",
    ptr = arr@ptr,
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' Length
#'
#' Returns the length of the input time series.
#'
#' @param arr TSA array with the time series.
#' @return The length of arr.
#' @export
Length <- function(arr) {
  try(out <- .C("length",
                ptr = arr@ptr,
                b = as.integer64(0),
                PACKAGE = package))
  
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' LinearTrend
#'
#' Calculates a linear least-squares regression for the values of the time series versus the sequence from 0 to
#' length of the time series minus one.
#'
#' @param arr TSA array with the time series.
#' @return List of TSA Array with:
#' {
#'     pvalue: The pvalues for all time series.
#'     rvalue: The rvalues for all time series.
#'     intercept: The intercept values for all time series.
#'     slope: The slope for all time series.
#'     stdrr: The stderr values for all time series.
#' }
#' @export
LinearTrend <- function(arr) {
  try(out <- .C(
    "linear_trend",
    ptr = arr@ptr,
    pvalue = as.integer64(0),
    rvalue = as.integer64(0),
    intercept = as.integer64(0),
    slope = as.integer64(0),
    stdrr = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  result <- list(
    "pvalue" = createArray(out$pvalue),
    "rvalue" = createArray(out$rvalue),
    "intercept" = createArray(out$intercept),
    "slope" = createArray(out$slope),
    "stdrr" = createArray(out$stdrr)
  )
  return(result)
}

#' Calculates all Local Maximals fot the time series in array.
#'
#' @param arr: TSA array with the time series.
#' @return: TSA array with the calculated local maximals for each time series in arr.
LocalMaximals <- function(arr) {
  try(out <-
        .C(
          "local_maximals",
          ptr = arr@ptr,
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' LongestStrikeAboveMean
#'
#' Calculates the length of the longest consecutive subsequence in arr that is bigger than the mean of arr.
#'
#' @param arr TSA array with the time series.
#' @return The length of the longest consecutive subsequence in the input time series that is bigger than the mean.
#' @export
LongestStrikeAboveMean <- function(arr) {
  try(out <-
        .C(
          "longest_strike_above_mean",
          ptr = arr@ptr,
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' LongestStrikeBelowMean
#'
#' Calculates the length of the longest consecutive subsequence in arr that is below the mean of arr.
#'
#' @param arr TSA array with the time series.
#' @return The length of the longest consecutive subsequence in the input time series that is below the mean.
#' @export
LongestStrikeBelowMean <- function(arr) {
  try(out <-
        .C(
          "longest_strike_below_mean",
          ptr = arr@ptr,
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
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
#'
#' @param arr TSA array with the time series.
#' @param m: Order of polynom to fit for estimating fixed points of dynamics.
#' @param r: Number of quantiles to use for averaging.
#' @return Largest fixed point of deterministic dynamics.
#' @export
MaxLangevinFixedPoint <- function(arr, m, r) {
  try(out <-
        .C(
          "max_langevin_fixed_point",
          ptr = arr@ptr,
          as.integer(m),
          as.single(r),
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' Mean
#'
#' Calculates the mean value for each time series within arr.
#'
#' @param arr TSA array with the time series.
#' @return The mean value of each time series within arr.
#' @export
Mean <- function(arr) {
  try(out <-
        .C("mean",
           ptr = arr@ptr,
           b = as.integer64(0),
           PACKAGE = package))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' Maximum
#'
#' Calculates the maximum value for each time series within arr.
#'
#' @param arr TSA Array of arrays of type double containing the time series.
#' @return The maximum value of each time series within arr.
#' @export
Maximum <- function(arr) {
  try(out <-
        .C("maximum",
           ptr = arr@ptr,
           b = as.integer64(0),
           PACKAGE = package))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' MeanAbsolutgeChange
#'
#' Calculates the mean over the absolute differences between subsequent time series values in arr.
#'
#' @param arr TSA array with the time series.
#' @return The mean over the absolute differences between subsequent time series values.
#' @export
MeanAbsoluteChange <- function(arr) {
  try(out <-
        .C(
          "mean_absolute_change",
          ptr = arr@ptr,
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' MeanChange
#'
#' Calculates the mean over the differences between subsequent time series values in arr.
#'
#' @param arr TSA array with the time series.
#' @return The mean over the differences between subsequent time series values.
#' @export
MeanChange <- function(arr) {
  try(out <-
        .C(
          "mean_change",
          ptr = arr@ptr,
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' MeanSecondDerivativeCentral
#'
#' Calculates mean value of a central approximation of the second derivative for each time series in arr.
#'
#' @param arr TSA array with the time series.
#' @return The mean value of a central approximation of
#' the second derivative for each time series.
#' @export
MeanSecondDerivativeCentral <- function(arr) {
  try(out <-
        .C(
          "mean_second_derivative_central",
          ptr = arr@ptr,
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' Median
#'
#' Calculates the median value for each time series within arr.
#'
#' @param arr TSA array with the time series.
#' @return The median value of each time series within arr.
#' @export
Median <- function(arr) {
  try(out <-
        .C("median",
           ptr = arr@ptr,
           b = as.integer64(0),
           PACKAGE = package))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' Minimum
#'
#' Calculates the minimum value for each time series within arr.
#'
#' @param arr TSA array with the time series.
#' @return The minimum value of each time series within arr.
#' @export
Minimum <- function(arr) {
  try(out <-
        .C("minimum",
           ptr = arr@ptr,
           b = as.integer64(0),
           PACKAGE = package))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' NumberCrossingM
#'
#' Calculates the number of m-crossings. A m-crossing is defined as two sequential values where the first
#' value is lower than m and the next is greater, or viceversa. If you set m to zero, you will get the number of
#' zero crossings.
#'
#' @param arr TSA array with the time series.
#' @param m The m value.
#' @return The number of m-crossings of each time series within arr.
#' @export
NumberCrossingM <- function(arr, m) {
  try(out <-
        .C(
          "number_crossing_m",
          ptr = arr@ptr,
          as.integer(m),
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' NumberCwtPeaks
#'
#' This feature calculator searches for different peaks. To do so, the time series is smoothed by a ricker
#' wavelet and for widths ranging from 1 to :math:'max_w`. This feature calculator returns the number of peaks that
#' occur at enough width scales and with sufficiently high Signal-to-Noise-Ratio (SNR).
#'
#' @param arr: TSA array with the time series.
#' @param max_w: The maximum width to consider.
#' @return: TSA array with the number of peaks for each time series.
#' @export
NumberCwtPeaks <- function(arr, max.w) {
  try(out <-
        .C(
          "number_cwt_peaks",
          ptr = arr@ptr,
          as.integer(max.w),
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' NumberPeaks
#'
#' Calculates the number of peaks of at least support \eqn{n} in the time series \eqn{arr}. A peak of support
#' \eqn{n} is defined as a subsequence of \eqn{arr} where a value occurs, which is bigger than
#' its \eqn{n} neighbours to the left and to the right.
#'
#' @param arr TSA array with the time series.
#' @param n The support of the peak.
#' @return The number of m-crossings of each time series within arr.
#' @export
NumberPeaks <- function(arr, n) {
  try(out <-
        .C(
          "number_peaks",
          ptr = arr@ptr,
          as.integer(n),
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  
  return(createArray(out$b))
}

#' PartialAutocorrelation
#'
#' Calculates the value of the partial autocorrelation function at the given lag. The lag \eqn{k}  partial
#' autocorrelation of a time series \eqn{\lbrace x_t, t = 1 \ldots T \rbrace} equals the partial correlation of
#' \eqn{x_t} and \eqn{x_{t-k}}, adjusted for the intermediate variables \eqn{\lbrace x_{t-1}, \ldots, x_{t-k+1}
#' \rbrace} ([1]). Following [2], it can be defined as:
#'
#' \deqn{
#'         \alpha_k = \frac{ Cov(x_t, x_{t-k} | x_{t-1}, \ldots, x_{t-k+1})}
#'         {\sqrt{ Var(x_t | x_{t-1}, \ldots, x_{t-k+1}) Var(x_{t-k} | x_{t-1}, \ldots, x_{t-k+1} )}}
#'}
#' with (a) \eqn{x_t = f(x_{t-1}, \ldots, x_{t-k+1})} and (b) \eqn{x_{t-k} = f(x_{t-1}, \ldots, x_{t-k+1})}
#' being AR(k-1) models that can be fitted by OLS. Be aware that in (a), the regression is done on past values to
#' predict \eqn{x_t} whereas in (b), future values are used to calculate the past value \eqn{x_{t-k}}.
#' It is said in [1] that "for an AR(p), the partial autocorrelations \eqn{\alpha_k} will be nonzero for \eqn{k<=p}
#' and zero for \eqn{k>p}."
#' With this property, it is used to determine the lag of an AR-Process.
#'
#' [1] Box, G. E., Jenkins, G. M., Reinsel, G. C., & Ljung, G. M. (2015).
#' Time series analysis: forecasting and control. John Wiley & Sons.
#' [2] https://onlinecourses.science.psu.edu/stat510/node/62
#'
#' @param arr: TSA array with the time series.
#' @param lags: TSA array with the lags to be calculated.
#' @return: TSA array with the partial autocorrelation for each time series for the given lag.
#' @export
PartialAutocorrelation <- function(arr, lags) {
  try(out <-
        .C(
          "partial_autocorrelation",
          ptr = arr@ptr,
          lags.ptr = lags@ptr,
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  eval.parent(substitute(lags@ptr <- out$lags.ptr))
  
  return(createArray(out$b))
}

#' PercentageOfReoccurringDatapointsToAllDatapoints
#'
#' Calculates the number of peaks of at least support \eqn{n} in the time series \eqn{arr}. A peak of support
#' \eqn{n} is defined as a subsequence of \eqn{arr} where a value occurs, which is bigger than
#' its \eqn{n} neighbours to the left and to the right.
#'
#' @param arr TSA array with the time series.
#' @param n The support of the peak.
#' @return The number of m-crossings of each time series within arr.
#' @export
PercentageOfReoccurringDatapointsToAllDatapoints <-
  function(arr, is.sorted) {
    try(out <-
          .C(
            "percentage_of_reoccurring_datapoints_to_all_datapoints",
            ptr = arr@ptr,
            as.logical(is.sorted),
            b = as.integer64(0),
            PACKAGE = package
          ))
    eval.parent(substitute(arr@ptr <- out$ptr))
    
    return(createArray(out$b))
  }

#' PercentageOfReoccurringValuesToAllValues
#'
#' Calculates the percentage of unique values, that are present in the time series more than once.
#'
#'\deqn{
#'   \frac{\textit{number of data points occurring more than once}}{\textit{number of all data points})}
#'}
#'
#' This means the percentage is normalized to the number of unique values, in contrast to the
#' PercentageOfReoccurringDatapointsToAllDatapoints.
#'
#' @param arr: TSA array with the time series.
#' @param is.sorted: Indicates if the input time series is sorted or not. Defaults to false.
#' @return: TSA array with the percentage of unique values, that are present in the time series more than once.
#' @export
PercentageOfReoccurringValuesToAllValues <-
  function(arr, is.sorted = FALSE) {
    try(out <-
          .C(
            "percentage_of_reoccurring_values_to_all_values",
            ptr = arr@ptr,
            as.logical(is.sorted),
            b = as.integer64(0),
            PACKAGE = package
          ))
    eval.parent(substitute(arr@ptr <- out$ptr))
    
    return(createArray(out$b))
  }

#' Quantile
#'
#' Returns values at the given quantile.
#'
#' @param arr TSA array with the time series.
#' @param q Percentile(s) at which to extract score(s). One or many.
#' @param precision Number of decimals expected.
#' @return The number of m-crossings of each time series within arr.
#' @export
Quantile <- function(arr, arr.q, precision = 1e8) {
  try(out <-
        .C(
          "quantile",
          arr.ptr = arr@ptr,
          arr.q.ptr = arr.q@ptr,
          as.single(precision),
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$arr.ptr))
  eval.parent(substitute(arr.q@ptr <- out$arr.q.ptr))
  
  return(createArray(out$b))
}

#' RangeCount
#'
#' Counts observed values within the interval [min, max).
#'
#' @param arr: TSA array with the time series.
#' @param min: Value that sets the lower limit.
#' @param max: Value that sets the upper limit.
#' @return: TSA array with the values at the given range.
#' @export
RangeCount <- function(arr, min, max) {
  try(out <-
        .C(
          "range_count",
          ptr = arr@ptr,
          as.integer(min),
          as.single(max),
          b = as.integer64(0),
          PACKAGE = package
        ))
  
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(createArray(out$b))
}

#' RatioBeyondRSigma
#'
#' Calculates the ratio of values that are more than  \eqn{r*std(x)} (so \eqn{r} sigma away from the mean of
#' \eqn{x}.
#'
#' @param arr TSA array with the time series.
#' @param q Percentile(s) at which to extract score(s). One or many.
#' @param precision Number of decimals expected.
#' @return The number of m-crossings of each time series within arr.
#' @export
RatioBeyondRSigma <- function(arr, r) {
  try(out <-
        .C(
          "ratio_beyond_r_sigma",
          ptr = arr@ptr,
          as.single(r),
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(createArray(out$b))
}

#' Calculates a factor which is 1 if all values in the time series occur only once, and below one if this is
#' not the case. In principle, it just returns:
#'
#' \deqn{
#'         \frac{\textit{number_unique_values}}{\textit{number_values}}
#' }
#' @param arr: TSA array with the time series.
#' @return: TSA array with the ratio of unique values with respect to the total number of values.
#' @export
RatioValueNumberToTimeSeriesLength <- function(arr) {
  try(out <-
        .C(
          "ratio_value_number_to_time_series_length",
          ptr = arr@ptr,
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(createArray(out$b))
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
#' @param  arr TSA array with the time series.
#' @return An array with the same dimensions as arr, whose values (time series in dimension 0)
#' contains the vectorized sample entropy for all the input time series in arr.
#' @export
SampleEntropy <- function(arr) {
  try(out <-
        .C(
          "sample_entropy",
          ptr = arr@ptr,
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(createArray(out$b))
}

#' Skewness
#'
#' Calculates the sample skewness of arr (calculated with the adjusted Fisher-Pearson standardized
#' moment coefficient G1).
#'
#' @param  arr TSA array with the time series.
#' @return Array containing the skewness of each time series in arr.
#' @export
Skewness <- function(arr) {
  try(out <-
        .C("skewness",
           ptr = arr@ptr,
           b = as.integer64(0),
           PACKAGE = package))
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(createArray(out$b))
}

#' SpktWelchDensity
#'
#' Estimates the cross power spectral density of the time series array at different frequencies. To do so, the
#' time series is first shifted from the time domain to the frequency domain.
#'
#' Welch's method computes an estimate of the power spectral density by dividing the data into overlapping
#' segments, computing a modified periodogram for each segment and averaging the periodograms.
#' [1] P. Welch, "The use of the fast Fourier transform for the estimation of power spectra: A method based on time
#' averaging over short, modified periodograms", IEEE Trans. Audio Electroacoust. vol. 15, pp. 70-73, 1967.
#' [2] M.S. Bartlett, "Periodogram Analysis and Continuous Spectra", Biometrika, vol. 37, pp. 1-16, 1950.
#' [3] Rabiner, Lawrence R., and B. Gold. "Theory and Application of Digital Signal Processing" Prentice-Hall, pp.
#' 414-419, 1975.
#'
#' @param arr: TSA array with the time series.
#' @param coeff: The coefficient to be returned.
#' @return: TSA array containing the power spectrum of the different frequencies for each time series in arr.
#' @export
SpktWelchDensity <- function(arr, coeff) {
  try(out <-
        .C(
          "spkt_welch_density",
          ptr = arr@ptr,
          as.integer(coeff),
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(createArray(out$b))
}

#' StandardDeviation
#'
#' Calculates the standard deviation of each time series within arr.
#'
#' @param  arr TSA array with the time series.
#' @return The standard deviation of each time series within arr.
#' @export
StandardDeviation <- function(arr) {
  try(out <-
        .C(
          "standard_deviation",
          ptr = arr@ptr,
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(createArray(out$b))
}

#' SumOfReoccuringDatapoints
#'
#' Calculates the sum of all data points, that are present in the time series more than once.
#'
#' @param  arr TSA array with the time series.
#' @return Returns the sum of all data points, that are present in the time series more than once.
#' @export
SumOfReoccurringDatapoints <- function(arr, is.sorted = FALSE) {
  try(out <-
        .C(
          "sum_of_reoccurring_datapoints",
          ptr = arr@ptr,
          as.logical(is.sorted),
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(createArray(out$b))
}

#' SumOfReoccurringValues
#'
#' Calculates the sum of all values, that are present in the time series more than once.
#'
#' @param arr: TSA array with the time series.
#' @param is.sorted: Indicates if the input time series is sorted or not. Defaults to false.
#' @return: TSA array with the sum of all values, that are present in the time series more than once.
#' @export
SumOfReoccurringValues <- function(arr, is.sorted = FALSE) {
  try(out <-
        .C(
          "sum_of_reoccurring_values",
          ptr = arr@ptr,
          as.logical(is.sorted),
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(createArray(out$b))
}

#' SumValues
#'
#' Calculates the sum over the time series arr.
#'
#' @param arr: TSA array with the time series.
#' @return: TSA array with the sum of values in each time series.
#' @export
SumValues <- function(arr) {
  try(out <-
        .C("sum_values",
           ptr = arr@ptr,
           b = as.integer64(0),
           PACKAGE = package))
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(createArray(out$b))
}

#' SymmetryLooking
#'
#' Calculates if the distribution of arr *looks symmetric*. This is the case if
#' \deqn{
#'  | mean(arr)-median(arr)| < r * (max(arr)-min(arr))
#' }
#'
#' @param arr TSA array with the time series.
#' @param r The percentage of the range to compare with.
#' @return An array denoting if the input time series look symmetric.
#' @export
SymmetryLooking <- function(arr, r) {
  try(out <-
        .C(
          "symmetry_looking",
          ptr = arr@ptr,
          as.single(r),
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(createArray(out$b))
}

#' TimeReversalAsymmetryStatistic
#'
#' This function calculates the value of:
#'
#' \deqn{
#'         \frac{1}{n-2lag} \sum_{i=0}^{n-2lag} x_{i + 2 \cdot lag}^2 \cdot x_{i + lag} - x_{i + lag} \cdot  x_{i}^2
#' }
#'which is
#'
#' \deqn{
#'        \mathbb{E}[L^2(X)^2 \cdot L(X) - L(X) \cdot X^2]
#' }
#' where \eqn{\mathbb{E}} is the mean and \eqn{L} is the lag operator. It was proposed in [1] as a promising
#' feature to extract from time series.
#'
#' @param arr: TSA array with the time series.
#' @param lag: The lag to be computed.
#' @return: TSA array containing the count of the given value in each time series.
#' @export
TimeReversalAsymmetryStatistic <- function(arr, lag) {
  try(out <-
        .C(
          "time_reversal_asymmetry_statistic",
          ptr = arr@ptr,
          as.integer(lag),
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(createArray(out$b))
}

#' ValueCount
#'
#' Counts occurrences of value in the time series arr.
#'
#' @param arr TSA array with the time series.
#' @param v The value to be counted.
#' @return An array containing the count of the given value in each time series.
#' @export
ValueCount <- function(arr, v) {
  try(out <-
        .C(
          "value_count",
          ptr = arr@ptr,
          as.single(v),
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(createArray(out$b))
}

#' Variance
#'
#' Computes the variance for the time series array.
#'
#' @param arr: TSA array with the time series.
#' @return: TSA array containing the variance in each time series.
#' @export
Variance <- function(arr) {
  try(out <-
        .C("variance",
           ptr = arr@ptr,
           b = as.integer64(0),
           PACKAGE = package))
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(createArray(out$b))
}

#' Variance Larger Than Standard Deviation
#'
#' Calculates if the variance of array is greater than the standard deviation. In other words, if the variance of
#' array is larger than 1.
#'
#' @param arr: TSA array with the time series.
#' @return: TSA array denoting if the variance of array is greater than the standard deviation.
#' @export
VarianceLargerThanStandardDeviation <- function(arr) {
  try(out <-
        .C(
          "variance_larger_than_standard_deviation",
          ptr = arr@ptr,
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(createArray(out$b))
}