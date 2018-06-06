#
#Copyright (c) 2018 Shapelets.io
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

#' Linear
#'
#' Calculates a linear least-squares regression for two sets of measurements. Both arrays should have the same
#' length.
#'
#' @param xss KHIVA array with the time series.
#' @param yss KHIVA array with the time series.
#' @return List with: {
#'         slope Slope of the regression line.
#'         intercept Intercept of the regression line.
#'         rvalue Correlation coefficient.
#'         pvalue Two-sided p-value for a hypothesis test whose null hypothesis is that the slope is zero, using Wald
#'                   Test with t-distribution of the test statistic.
#'         stderrest Standard error of the estimated gradient.
#' }
#' @export
Linear <- function(xss, yss) {
  try(out <- .C(
    "linear",
    xss.ptr = xss@ptr,
    yss.ptr = yss@ptr,
    slope = as.integer64(0),
    intercept = as.integer64(0),
    rvalue = as.integer64(0),
    pvalue = as.integer64(0),
    stderrest = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(xss@ptr <- out$xss.ptr))
  eval.parent(substitute(yss@ptr <- out$yss.ptr))
  
  result <- list(
    "slope" = createArray(out$slope),
    "intercept" = createArray(out$intercept),
    "rvalue" = createArray(out$rvalue),
    "pvalue" = createArray(out$pvalue),
    "stderrest" = createArray(out$stderrest)
  )
  return(result)
}