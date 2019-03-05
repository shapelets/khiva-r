#
#Copyright (c) 2019 Shapelets.io
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

#' KMeans
#'
#' Calculates the K-Means algorithm.
#'
#' [1] S. Lloyd. 1982. Least squares quantization in PCM. IEEE Transactions on Information Theory, 28, 2,
#' Pages 129-137.
#'
#' @param tss Expects an input array whose dimension zero is the length of the time series (all the same) and
#' dimension one indicates the number of time series.
#' @param k The number of means to be computed.
#' @param tolerance The error tolerance to stop the computation of the centroids.
#' @param maxIterations The maximum number of iterations allowed.
#'
#' @return List with the kMeans centroids as first element and kMeans labels as second element.
#' @export
KMeans <-
  function(tss,
           k,
           tolerance = 1e-10,
           maxIterations = 100) {
    try(out <- .C(
      "k_means",
      tss.ptr = tss@ptr,
      as.integer(k),
      centroids = as.integer64(0),
      labels = as.integer64(0),
      as.single(tolerance),
      as.integer(maxIterations),
      PACKAGE = package
    ))
    
    eval.parent(substitute(tss@ptr <- out$tss.ptr))
    
    newList <-
      list("centroids" = createArray(out$centroids),
           "labels" = createArray(out$labels))
    
    return(newList)
  }

#' KShape
#'
#' Calculates the k-shape algorithm.
#'
#' [1] John Paparrizos and Luis Gravano. 2016. k-Shape: Efficient and Accurate Clustering of Time Series.
#' SIGMOD Rec. 45, 1 (June 2016), 69-76.
#'
#' @param tss Expects an input array whose dimension zero is the length of the time series (all the same) and
#' dimension one indicates the number of time series.
#' @param k The number of means to be computed.
#' @param tolerance The error tolerance to stop the computation of the centroids.
#' @param maxIterations The maximum number of iterations allowed.
#'
#' @return List with the k-shape centroids as first element and k-shape labels as second element.
#' @export
KShape <-
  function(tss,
           k,
           tolerance = 1e-10,
           maxIterations = 100) {
    try(out <- .C(
      "k_shape",
      tss.ptr = tss@ptr,
      as.integer(k),
      centroids = as.integer64(0),
      labels = as.integer64(0),
      as.single(tolerance),
      as.integer(maxIterations),
      PACKAGE = package
    ))
    
    eval.parent(substitute(tss@ptr <- out$tss.ptr))
    
    newList <-
      list("centroids" = createArray(out$centroids),
           "labels" = createArray(out$labels))
    
    return(newList)
  }
