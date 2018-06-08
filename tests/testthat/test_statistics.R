#
#Copyright (c) 2018 Shapelets.io
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

test_that("Test CovarianceStatistics Unbiased", {
  ta <- as.single(c(-2.1,-1, 4.3, 3, 1.1, 0.12, 3, 1.1, 0.12))
  a <- Array(array(ta, dim = c(3, 3)))
  out <- CovarianceStatistics(a, TRUE)
  b <- c(getData(out))
  expected <- c(
    11.70999999,
    -4.286,
    -4.286,
    -4.286,
    2.14413333,
    2.14413333,
    -4.286,
    2.14413333,
    2.14413333
  )
  expect_equal(b, expected, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

test_that("Test CovarianceStatisticsUnbiased", {
  ta <- as.single(c(-2.1,-1, 4.3, 3, 1.1, 0.12, 3, 1.1, 0.12))
  a <- Array(array(ta, dim = c(3, 3)))
  out <- CovarianceStatistics(a, FALSE)
  b <- c(getData(out))
  expected <-
    c(
      7.80666667,
      -2.85733333,
      -2.85733333,
      -2.85733333,
      1.42942222,
      1.42942222,
      -2.85733333,
      1.42942222,
      1.42942222
    )
  expect_equal(b, expected, 1e-6)

  deleteArray(a)
  deleteArray(out)
})

test_that("Test KurtosisStatistics", {
  ta <-
    as.single(c(0, 1, 2, 3, 4, 5, 2, 2, 2, 20, 30, 25))
  a <- Array(array(ta, dim = c(6, 2)))
  out <- KurtosisStatistics(a)
  b <- c(getData(out))
  expected = c(-1.2,-2.66226722)
  expect_equal(b, expected, 1e-2)
  deleteArray(a)
  deleteArray(out)
})

test_that("Test LjungBox", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5, 6, 7))
  a <- Array(array(ta, dim = c(4, 2)))
  out <- LjungBox(a, 3)
  b <- c(getData(out))
  expected <-
    c(6.4400, 6.4400)
  expect_equal(b, expected, 1e-2)
  deleteArray(a)
  deleteArray(out)
})

test_that("Test MomentStatistics", {
  ta <-
    as.single(c(0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5))
  a <- Array(array(ta, dim = c(6, 2)))
  out <- MomentStatistics(a, 2)
  b <- c(getData(out))
  expected = c(9.166666666, 9.166666666)
  expect_equal(b, expected, 1e-6)
  out.four <- MomentStatistics(a, 4)
  b <- c(getData(out.four))
  expected = c(163.1666666666, 163.1666666666)
  expect_equal(b, expected, 1e-6)
  deleteArray(a)
  deleteArray(out)
  deleteArray(out.four)
})

test_that("Test QuantileStatistics", {
  ta <-
    as.single(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
  a <- Array(array(ta, dim = c(6, 2)))
  tq <- as.single(c(0.1, 0.2))
  q <- Array(array(tq, dim = c(2, 1)))
  out <- QuantileStatistics(a, q)
  b <- c(getData(out))
  expected = c(0.5, 1.0, 6.5, 7.0)
  expect_equal(b, expected, 1e-2)
  deleteArray(a)
  deleteArray(q)
  deleteArray(out)
})

test_that("Test QuantileCutStatistics2", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
  a <- Array(array(ta, dim = c(6, 2)))
  out <- QuantilesCutStatistics(a, 2)
  b <- c(getData(out))
  expected <-
    c(
      -1.0e-8,
      -1.0e-8,
      -1.0e-8,
      2.5,
      2.5,
      2.5,
      2.5,
      2.5,
      2.5,
      5.0,
      5.0,
      5.0,
      6.0,
      6.0,
      6.0,
      8.5,
      8.5,
      8.5,
      8.5,
      8.5,
      8.5,
      11.0,
      11.0,
      11.0
    )
  expect_equal(b, expected, 1e-2)
  deleteArray(a)
  deleteArray(out)
})

test_that("Test QuantileCutStatistics3", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
  a <- Array(array(ta, dim = c(6, 2)))
  out <- QuantilesCutStatistics(a, 3)
  b <- c(getData(out))
  expected <-
    c(
      -1.0e-8,
      -1.0e-8,
      1.6666667,
      1.6666667,
      3.3333335,
      3.3333335,
      1.6666667,
      1.6666667,
      3.3333335,
      3.3333335,
      5.0,
      5.0,
      6.0,
      6.0,
      7.6666665,
      7.6666665,
      9.333333,
      9.333333,
      7.6666665,
      7.6666665,
      9.333333,
      9.333333,
      11.0,
      11.0
    )
  expect_equal(b, expected, 1e-2)
  deleteArray(a)
  deleteArray(out)
})

test_that("Test QuantileCutStatistics7", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
  a <- Array(array(ta, dim = c(6, 2)))
  out <- QuantilesCutStatistics(a, 7)
  b <- c(getData(out))
  expected <-
    c(
      -1.0e-8,
      0.71428573,
      1.4285715,
      2.857143,
      3.5714288,
      4.2857146,
      0.71428573,
      1.4285715,
      2.1428573,
      3.5714288,
      4.2857146,
      5.0,
      6.0,
      6.714286,
      7.4285717,
      8.857143,
      9.571428,
      10.285714,
      6.714286,
      7.4285717,
      8.142858,
      9.571428,
      10.285714,
      11.0
    )
  expect_equal(b, expected, 1e-2)
  deleteArray(a)
  deleteArray(out)
})

test_that("Test SampleStdevStatistics", {
  ta <-
    as.single(c(0, 1, 2, 3, 4, 5, 2, 2, 2, 20, 30, 25))
  a <- Array(array(ta, dim = c(6, 2)))
  out <- SampleStdevStatistics(a)
  b <- c(getData(out))
  expected = c(1.870828693, 12.988456413)
  expect_equal(b, expected, 1e-2)
  deleteArray(a)
  deleteArray(out)
})

test_that("Test SkewnessStatistics", {
  ta <-
    as.single(c(0, 1, 2, 3, 4, 5, 2, 2, 2, 20, 30, 25))
  a <- Array(array(ta, dim = c(6, 2)))
  out <- SkewnessStatistics(a)
  b <- c(getData(out))
  expected = c(0.0, 0.236177069879499)
  expect_equal(b, expected, 1e-2)
  deleteArray(a)
  deleteArray(out)
})
