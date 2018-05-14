#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

#' Paa
#'
#' Piecewise Aggregate Approximation (PAA) approximates a time series \eqn{X} of length \eqn{n} into vector
#' \eqn{\bar{X}=(\bar{x}_{1},…,\bar{x}_{M})} of any arbitrary length \eqn{M \leq n} where each of \eqn{\bar{x_{i}}} is
#' calculated as follows:
#' \deqn{
#' \bar{x}_{i} = \frac{M}{n} \sum_{j=n/M(i-1)+1}^{(n/M)i} x_{j}.
#' }
#'
#' @param arr TSA Array with the x-coordinates and y-coordinates of the input points (x in column 0 and y in column 1).
#' @param bins Sets the total number of divisions.
#'
#' @return Array of points with the reduced dimensionality.
#' @export
Paa <- function(arr, bins) {
  try(out <-
        .C(
          "paa",
          ptr = arr@ptr,
          as.integer(bins),
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(createArray(out$b))
}

#' Pip
#'
#' Calculates the number of Perceptually Important Points (PIP) in the time series.
#'
#' [1] Fu TC, Chung FL, Luk R, and Ng CM. Representing financial time series based on data point importance.
#' Engineering Applications of Artificial Intelligence, 21(2):277-300, 2008.
#'
#' @param arr TSA Array whose dimension zero is the length of the time series.
#' @param numberIPs The number of points to be returned.
#'
#' @return Array with the most Perceptually Important numberIPs.
#' @export
Pip <- function(arr, numberIPs) {
  try(out <-
        .C(
          "pip",
          ptr = arr@ptr,
          as.integer(numberIPs),
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(createArray(out$b))
}

#' PLABottomUp
#'
#' Applies the Piecewise Linear Approximation (PLA BottomUP) to the time series.
#'
#' [1] Zhu Y, Wu D, Li Sh (2007). A Piecewise Linear Representation Method of Time Series Based on Feature Points.
#' Knowledge-Based Intelligent Information and Engineering Systems 4693:1066-1072.
#'
#' @param ts Expects a tsa array containing the set of points to be reduced. The first component of the points in
#' the first column and the second component of the points in the second column.
#' @param max.error The maximum approximation error allowed.
#'
#' @return The reduced number of points.
#' @export
PLABottomUp <- function(ts, max.error) {
  try(out <-
        .C(
          "pla_bottom_up",
          ptr = ts@ptr,
          as.single(max.error),
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(ts@ptr <- out$ptr))
  return(createArray(out$b))
}

#' PLASlidingWindow
#'
#' Applies the Piecewise Linear Approximation (PLA SlidingWindow) to the time series.
#'
#' [1] Zhu Y, Wu D, Li Sh (2007). A Piecewise Linear Representation Method of Time Series Based on Feature Points.
#' Knowledge-Based Intelligent Information and Engineering Systems 4693:1066-1072.
#'
#' @param ts Expects a tsa array containing the set of points to be reduced. The first component of the points in
#' the first column and the second component of the points in the second column.
#' @param max.error The maximum approximation error allowed.
#'
#' @return The reduced number of points.
#' @export
PLASlidingWindow <- function(ts, max.error) {
  try(out <-
        .C(
          "pla_sliding_window",
          ptr = ts@ptr,
          as.single(max.error),
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(ts@ptr <- out$ptr))
  return(createArray(out$b))
}

#' RamerDouglasPeucker
#'
#' The Ramer–Douglas–Peucker algorithm (RDP) is an algorithm for reducing the number of points in a curve that is
#' approximated by a series of points. It reduces a set of points depending on the perpendicular distance of the points
#' and epsilon, the greater epsilon, more points are deleted.
#'
#' [1] Urs Ramer, "An iterative procedure for the polygonal approximation of plane curves", Computer Graphics and Image
#' Processing, 1(3), 244–256 (1972) doi:10.1016/S0146-664X(72)80017-0.
#'
#' [2] David Douglas & Thomas Peucker, "Algorithms for the reduction of the number of points required to represent a
#' digitized line or its caricature", The Canadian Cartographer 10(2), 112–122 (1973) doi:10.3138/FM57-6770-U75U-7727
#'
#' @param arr TSA Array with with the x-coordinates and y-coordinates of the input points (x in column 0 and y in column 1).
#' @param epsilon It acts as the threshold value to decide which points should be considered meaningful or not.
#'
#' @return Array with the x-coordinates and y-coordinates of the selected points (x in column 0 and y in
#' column 1).
#' @export
RamerDouglasPeucker <- function(arr, epsilon) {
  try(out <-
        .C(
          "ramer_douglas_peucker",
          ptr = arr@ptr,
          as.double(epsilon),
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(createArray(out$b))
}

#' Sax
#'
#' Symbolic Aggregate approXimation (SAX). It transforms a numeric time series into a time series of symbols with
#' the same size. The algorithm was proposed by Lin et al.) and extends the PAA-based approach inheriting the original
#' algorithm simplicity and low computational complexity while providing satisfactory sensitivity and selectivity in
#' range query processing. Moreover, the use of a symbolic representation opened a door to the existing wealth of
#' data-structures and string-manipulation algorithms in computer science such as hashing, regular expression, pattern
#' matching, suffix trees, and grammatical inference.
#'
#' [1] Lin, J., Keogh, E., Lonardi, S. & Chiu, B. (2003) A Symbolic Representation of Time Series, with Implications for
#' Streaming Algorithms. In proceedings of the 8th ACM SIGMOD Workshop on Research Issues in Data Mining and Knowledge
#' Discovery. San Diego, CA. June 13.
#'
#' @param arr TSA Array with the input time series.
#' @param alphabetSize Number of element within the alphabet.
#'
#' @return Array of symbols.
#' @export
Sax <- function(arr, alphabetSize) {
  try(out <-
        .C(
          "sax",
          ptr = arr@ptr,
          as.integer(alphabetSize),
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(createArray(out$b))
}

#' Visvalingam
#'
#' Reduces a set of points by applying the Visvalingam method (minimun triangle area) until the number
#' of points is reduced to numPoints.
#''
#' [1] M. Visvalingam and J. D. Whyatt, Line generalisation by repeated elimination of points,
#' The Cartographic Journal, 1993.
#'
#' @param arr TSA Array with the x-coordinates and y-coordinates of the input points (x in column 0 and y in column 1).
#' @param numPoints Sets the number of points returned after the execution of the method.
#'
#' @return Array with the x-coordinates and y-coordinates of the selected points (x in column 0 and y in
#' column 1).
#' @export
Visvalingam <- function(arr, numPoints) {
  try(out <-
        .C(
          "visvalingam",
          ptr = arr@ptr,
          as.integer(numPoints),
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(createArray(out$b))
}