#
#Copyright (c) 2018 Shapelets.io
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

#' Polyfit
#'
#' Least squares polynomial fit. Fit a polynomial \eqn{p(x) = p[0] * x^{deg} + ... + p[deg]} of degree
#' \eqn{deg} to points \eqn{(x, y)}. Returns a vector of coefficients \eqn{p} that minimises the squared error.
#'
#' @param x KHIVA array with the x-coordinates of the M sample points \eqn{(x[i], y[i])}.
#' @param y KHIVA array with the y-coordinates of the sample points.
#' @param deg Degree of the fitting polynomial
#' @return: KHIVA array with the polynomial coefficients, highest power first.
#' @export
Polyfit <- function(x, y, deg) {
  try(out <- .C(
    "polyfit",
    x.ptr = x@ptr,
    y.ptr = y@ptr,
    as.integer(deg),
    b = as.integer64(0),
    PACKAGE = package
  ))
  eval.parent(substitute(x@ptr <- out$x.ptr))
  eval.parent(substitute(y@ptr <- out$y.ptr))
  
  return(createArray(out$b))
}

#' Roots
#'
#' Calculates the roots of a polynomial with coefficients given in \eqn{p}. The values in the rank-1 array
#' \eqn{p} are coefficients of a polynomial. If the length of \eqn{p} is \eqn{n+1} then the polynomial is
#' described by:
#' \deqn{
#' p[0] * x^n + p[1] * x^{n-1} + ... + p[n-1] * x + p[n]
#' }
#'
#' @param p KHIVA array with the polynomial coefficients.
#' @return KHIVA array with the roots of the polynomial.
#' @export
Roots <- function(p) {
  try(out <- .C("roots",
                ptr = p@ptr,
                b = as.integer64(0),
                PACKAGE = package))
  eval.parent(substitute(p@ptr <- out$ptr))
  
  return(createArray(out$b))
}