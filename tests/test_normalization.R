#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

test_that("Test Znorm", {
  SetBackend(4)
  SetDevice(0)
  
  ta <-
    as.double(c(0, 1, 2, 3))
  tb <-
    as.double(c(4, 5, 6, 7))
  
  a <- Array(array(c(ta, tb), dim = c(4, 2)))
  out <- Znorm(a)
  b <- c(getData(out))
  expected <-
    as.double(c(
      -1.341640786499870,
      -0.447213595499958,
      0.447213595499958,
      1.341640786499870
    ))
  for (i in 1:4) {
    expect_equal(b[i], expected[i], 1e-3)
    expect_equal(b[i + 4], expected[i], 1e-3)
  }
})

test_that("Test ZnormInPlace", {
  ta <- as.single(c(0, 1, 2, 3))
  tb <- as.single(c(4, 5, 6, 7))
  expected = (
    c(
      -1.341640786499870,-0.447213595499958,
      0.447213595499958,
      1.341640786499870,
      -1.341640786499870,-0.447213595499958,
      .447213595499958,
      1.341640786499870
    )
  )
  a <- Array(array(c(ta, tb), dim = c(4, 2)))
  ZnormInPlace(a)
  b <- c(getData(a))
  expect_equal(b, expected, 1e-6)
})

test_that("Test MaxMinNorm", {
  ta <- as.single(c(0, 1, 2, 3))
  tb <- as.single(c(4, 5, 6, 7))
  expected <-
    (c(
      1.0,
      1.3333333333333,
      1.66666667,
      2.0,
      1.0,
      1.3333333333333,
      1.66666667,
      2.0
    ))
  a <- Array(array(c(ta, tb), dim = c(4, 2)))
  out <- MaxMinNorm(a, 2, 1)
  b <- c(getData(out))
  expect_equal(b, expected, 1e-6)
})

test_that("Test MaxMinNormInPlace", {
  ta <- as.single(c(0, 1, 2, 3))
  tb <- as.single(c(4, 5, 6, 7))
  expected <-
    (c(
      1.0,
      1.3333333333333,
      1.66666667,
      2.0,
      1.0,
      1.3333333333333,
      1.66666667,
      2.0
    ))
  a <- Array(array(c(ta, tb), dim = c(4, 2)))
  MaxMinNormInPlace(a, 2, 1)
  b <- c(getData(a))
  expect_equal(b, expected, 1e-6)
})

test_that("Test DecimalScalingNorm", {
  ta <- as.single(c(0, 1, -2, 3))
  tb <- as.single(c(40, 50, 60, -70))
  expected <- c(0.0, 0.1, -0.2, 0.3, 0.4, 0.5, 0.6, -0.7)
  a <- Array(array(c(ta, tb), dim = c(4, 2)))
  out <- DecimalScalingNorm(a)
  b <- c(getData(out))
  expect_equal(b, expected, 1e-6)
})

test_that("Test DecimalScalingNormInPlace", {
  ta <- as.single(c(0, 1, -2, 3))
  tb <- as.single(c(40, 50, 60, -70))
  expected <- (c(0.0, 0.1, -0.2, 0.3, 0.4, 0.5, 0.6, -0.7))
  a <- Array(array(c(ta, tb), dim = c(4, 2)))
  DecimalScalingNormInPlace(a)
  b <- c(getData(a))
  expect_equal(b, expected, 1e-6)
})

test_that("Test MeanNorm", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5, 6, 7))
  a <- Array(array(ta, dim = c(4, 2)))
  expected <- (c(-0.5, -0.166666667, 0.166666667, 0.5, -0.5, -0.166666667, 0.166666667, 0.5))
  out <- MeanNorm(a)
  b <- c(getData(out))
  expect_equal(b, expected, 1e-6)
})

test_that("Test MeanNormInPlace", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5, 6, 7))
  a <- Array(array(ta, dim = c(4, 2)))
  expected <- (c(-0.5, -0.166666667, 0.166666667, 0.5, -0.5, -0.166666667, 0.166666667, 0.5))
  MeanNormInPlace(a)
  b <- c(getData(a))
  expect_equal(b, expected, 1e-6)
})
