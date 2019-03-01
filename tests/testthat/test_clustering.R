#
#Copyright (c) 2018 Shapelets.io
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

testthat::setup(
  SetBackend(KHIVABackend()$KHIVA_BACKEND_CPU)
)

context("Khiva kmeans tests")

test_that("Test KMeans", {
  ta <- as.single(c(0.0,   1.0,  2.0,  3.0))
  tb <- as.single(c(6.0,   7.0,  8.0,  9.0))
  tc <- as.single(c(2.0,-2.0,  4.0,-4.0))
  td <- as.single(c(8.0,   5.0,  3.0,  1.0))
  te <- as.single(c(15.0, 10.0,  5.0,  0.0))
  tf <- as.single(c(7.0,-7.0,  1.0,-1.0))
  a <- Array(array(c(ta, tb, tc, td, te, tf), dim = c(4, 6)))
  
  expected.centroids <-
    sort(c(array(as.double(
      c(
        0.0,
        0.1667,
        0.3333,
        0.5,
        1.5,
        -1.5,
        0.8333,
        -0.8333,
        4.8333,
        3.6667,
        2.6667,
        1.6667
      )
    ), dim = c(4, 3))))
  
  out <- KMeans(a, 3)
  centroids <- sort(c(getData(out$centroids)))
  for (i in 1:length(expected.centroids)) {
    expect_equal(centroids[i], expected.centroids[i], 1e-2)
  }
  
  deleteArray(a)
  deleteArray(out[[1]])
  deleteArray(out[[2]])
})

context("Khiva kshape tests")

test_that("Test KShape", {
  ta <- as.single(c(1.0,   2.0,   3.0,  4.0,  5.0,  6.0, 7.0))
  tb <- as.single(c(0.0,  10.0,   4.0,  5.0,  7.0,-3.0, 0.0))
  tc <- as.single(c(-1.0, 15.0,-12.0,  8.0,  9.0,  4.0, 5.0))
  td <- as.single(c(2.0,   8.0,   7.0,-6.0,-1.0,  2.0, 9.0))
  te <- as.single(c(-5.0,-5.0,-6.0,  7.0,  9.0,  9.0, 0.0))
  a <- Array(array(c(ta, tb, tc, td, te), dim = c(7, 5)))
  
  expected.centroids <-
    sort(c(array(as.double(
      c(
        -0.5234,
        0.1560,
        -0.3627,
        -1.2764,
        -0.7781,
        0.9135,
        1.8711,-0.7825,
        1.5990,
        0.1701,
        0.4082,
        0.8845,
        -1.4969,
        -0.7825,-0.6278,
        1.3812,
        -2.0090,
        0.5022,
        0.6278,
        0.0000,
        0.1256
      )
    ), dim = c(7, 3))))
  
  out <- KShape(a, 3)
  centroids <- sort(c(getData(out$centroids)))
  labels <- c(getData(out$labels))
  expected.labels <-
    as.integer(c(0, 1, 2, 0, 0))
  for (i in 1:length(expected.labels)) {
    expect_equal(labels[i], expected.labels[i], 1e-2)
  }
  for (i in 1:length(expected.centroids)) {
    expect_equal(centroids[i], expected.centroids[i], 1e-2)
  }
  
  deleteArray(a)
  deleteArray(out[[1]])
  deleteArray(out[[2]])
})