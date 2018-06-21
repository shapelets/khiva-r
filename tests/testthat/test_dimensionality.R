#
#Copyright (c) 2018 Shapelets.io
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

testthat::setup(
  SetBackend(KHIVABackend()$KHIVA_BACKEND_CPU)
)

context("Khiva R tests")

test_that("Test RamerDouglasPeucker", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
  tb <-
    as.single(c(0, 0.1, -0.1, 5.0, 6.0, 7.0, 8.1, 9.0, 9.0, 9.0))
  
  a <- Array(array(c(ta, tb), dim = c(10, 2)))
  out <- RamerDouglasPeucker(a, 1.0)
  b <- c(getData(out))
  expected <- as.double(c(0, 2, 3, 6, 9, 0, -0.1, 5.0, 8.1, 9.0))
  expect_equal(b, expected, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

test_that("Test Visvalingam", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
  tb <-
    as.single(c(0, 0.1, -0.1, 5.0, 6.0, 7.0, 8.1, 9.0, 9.0, 9.0))
  
  a <- Array(array(c(ta, tb), dim = c(10, 2)))
  out <- Visvalingam(a, 5)
  b <- c(getData(out))
  expected <- as.double(c(0, 2, 5, 7, 9, 0, -0.1, 7.0, 9.0, 9.0))
  expect_equal(b, expected, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

test_that("Test Paa", {
  ta <-
    as.single(c(0.0, 0.1, -0.1, 5.0, 6.0, 7.0, 8.1, 9.0, 9.0, 9.0))
  tb <-
    as.single(c(0.0, 0.1, -0.1, 5.0, 6.0, 7.0, 8.1, 9.0, 9.0, 9.0))
  
  a <- Array(array(c(ta, tb), dim = c(10, 2)))
  out <- Paa(a, 5)
  b <- c(getData(out))
  expected <-
    as.double(c(0.05, 2.45, 6.5, 8.55, 9.0, 0.05, 2.45, 6.5, 8.55, 9.0))
  expect_equal(b, expected, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

test_that("Test Sax", {
  # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.info()['sysname']
  if (travis.platform != 'Darwin') {
  ta <- as.single(c(0.05, 2.45, 6.5, 8.55, 9.0))
  tb <- as.single(c(0.05, 2.45, 6.5, 8.55, 9.0))
  
  a <- Array(array(c(ta, tb), dim = c(5, 2)))
  out <- Sax(a, 3)
  b <- c(getData(out))
  expected <- as.integer(c(0, 0, 1, 2, 2, 0, 0, 1, 2, 2))
  expect_equal(b, expected)
  deleteArray(a)
  deleteArray(out)
  }
})

test_that("Test Pip", {
  ta <-
    as.single(c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0))
  tb <-
    as.single(c(0.0, 0.1, -0.1, 5.0, 6.0, 7.0, 8.1, 9.0, 9.0, 9.0))
  
  a <- Array(array(c(ta, tb), dim = c(10, 2)))
  out <- Pip(a, 6)
  b <- c(getData(out))
  expected <-
    as.double(c(0.0, 2.0, 3.0, 6.0, 7.0, 9.0, 0.0, -0.1, 5.0, 8.1, 9.0, 9.0))
  expect_equal(b, expected, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

test_that("Test PLABottomUp", {
  x <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)
  y <- c(0.0, 0.1, -0.1, 5.0, 6.0, 7.0, 8.1, 9.0, 9.0, 9.0)
  expected = c(0, 1, 2, 3, 4, 7, 8, 9, 0, 0.1, -0.1, 5, 6, 9, 9, 9)
  a <- Array(array(c(x, y), dim = c(10, 2)))
  out <- PLABottomUp(a, 1)
  b <- c(getData(out))
  expect_equal(b, expected, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

test_that("Test PLASlidingWindow", {
  x <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)
  y <- c(0.0, 0.1, -0.1, 5.0, 6.0, 7.0, 8.1, 9.0, 9.0, 9.0)
  expected = c(0, 2, 3, 7, 8, 9, 0, -0.1, 5, 9, 9, 9)
  a <- Array(array(c(x, y), dim = c(10, 2)))
  out <- PLASlidingWindow(a, 1)
  b <- c(getData(out))
  expect_equal(b, expected, 1e-6)
  deleteArray(a)
  deleteArray(out)
})