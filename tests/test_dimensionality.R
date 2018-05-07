#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

test_that("Test RamerDouglasPeucker", {
  SetBackend(4)
  SetDevice(0)
  
  ta <- as.single(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
  tb <-
    as.single(c(0, 0.1, -0.1, 5.0, 6.0, 7.0, 8.1, 9.0, 9.0, 9.0))
  
  a <- Array(array(c(ta, tb), dim = c(10, 2)))
  out <- RamerDouglasPeucker(a, 1.0)
  b <- c(getData(out))
  expected <- as.double(c(0, 2, 3, 6, 9, 0, -0.1, 5.0, 8.1, 9.0))
  expect_equal(b, expected, 1e-6)
})

test_that("Test Visvalingam", {
  SetBackend(4)
  SetDevice(0)
  
  ta <- as.single(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
  tb <-
    as.single(c(0, 0.1, -0.1, 5.0, 6.0, 7.0, 8.1, 9.0, 9.0, 9.0))
  
  a <- Array(array(c(ta, tb), dim = c(10, 2)))
  out <- Visvalingam(a, 5)
  b <- c(getData(out))
  expected <- as.double(c(0, 2, 5, 7, 9, 0, -0.1, 7.0, 9.0, 9.0))
  expect_equal(b, expected, 1e-6)
})

test_that("Test Paa", {
  SetBackend(4)
  SetDevice(0)
  
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
})

test_that("Test Sax", {
  SetBackend(4)
  SetDevice(0)
  
  ta <- as.single(c(0.05, 2.45, 6.5, 8.55, 9.0))
  tb <- as.single(c(0.05, 2.45, 6.5, 8.55, 9.0))
  
  a <- Array(array(c(ta, tb), dim = c(5, 2)))
  out <- Sax(a, 3)
  b <- c(getData(out))
  expected <- as.integer(c(0, 0, 1, 2, 2, 0, 0, 1, 2, 2))
  expect_equal(b, expected)
})

test_that("Test Pip", {
  SetBackend(4)
  SetDevice(0)
  
  ta <-
    as.single(c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0))
  tb <-
    as.single(c(0.0, 0.1, -0.1, 5.0, 6.0, 7.0, 8.1, 9.0, 9.0, 9.0))
  
  a <- Array(array(c(ta, tb), dim = c(10, 2)))
  out <- Pip(a, 6)
  b <- c(getData(out))
  expected <-
    as.double(c(0.0, 2.0, 4.0, 5.0, 6.0, 9.0, 0.0, -0.1, 6.0, 7.0, 8.1, 9.0))
  expect_equal(b, expected, 1e-6)
})
