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
#
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
  function(arr, isSorted) {
    try(out <-
          .C(
            "percentage_of_reoccurring_datapoints_to_all_datapoints",
            ptr = arr@ptr,
            as.logical(isSorted),
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

#' Skewness
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