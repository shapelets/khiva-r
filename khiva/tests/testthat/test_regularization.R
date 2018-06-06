#
#Copyright (c) 2018 Shapelets.io
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

test_that("Test GroupBySingleColumn", {
  a <-
    Array(array(as.single(c(
      0, 1, 1, 2, 2, 3, 0, 3, 3, 1, 1, 2
    )), dim = c(6, 2)))
  expected <- (c(0, 3, 1, 2))
  out <- GroupBy(a, 0)
  b <- c(getData(out))
  expect_equal(b, expected, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

test_that("Test GroupByDoubleKeyColumn", {
  a <-
    Array(array(as.single(
      c(0, 1, 1, 2, 2, 3, 1, 2, 2, 3, 3, 4, 0, 3, 3, 1, 1, 2)
    ), dim = c(6, 3)))
  expected <- (c(0, 3, 1, 2))
  out <- GroupBy(a, 0, 2)
  b <- c(getData(out))
  expect_equal(b, expected, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

test_that("Test GroupByDoubleKeyColumn2", {
  a <-
    Array(array(as.single(c(
      0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 2, 3, 4, 5
    )), dim = c(5, 3)))
  expected <- (c(1, 2, 3.5, 5))
  out <- GroupBy(a, 0, 2)
  b <- c(getData(out))
  expect_equal(b, expected, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

test_that("Test GroupByDoubleKeyDoubleValueColumn", {
  a <-
    Array(array(as.single(
      c(0, 0, 0, 2, 2, 2, 2, 2, 4, 4, 0, 1, 2, 3, 4, 1, 1, 1, 1, 1)
    ), dim = c(5, 4)))
  expected <- (c(1, 3.5, 1, 1))
  out <- GroupBy(a, 0, 2, 2)
  b <- c(getData(out))
  expect_equal(b, expected, 1e-6)
  deleteArray(a)
  deleteArray(out)
})
