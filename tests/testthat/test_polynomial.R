#
#Copyright (c) 2019 Shapelets.io
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

testthat::setup(
  SetBackend(KHIVABackend()$KHIVA_BACKEND_CPU)
)

context("Khiva polyfit1 tests")

test_that("Test polyfit1", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5))
  tb <- as.single(c(0, 1, 2, 3, 4, 5))
  expected <-  c(1.0, 0.0)
  
  a <- Array(array(ta, dim = c(6, 1)))
  b <- Array(array(tb, dim = c(6, 1)))
  
  out <- Polyfit(a, b, 1)
  c <- c(getData(out))
  expect_equal(c, expected, 1e-5)
  deleteArray(a)
  deleteArray(b)
  deleteArray(out)
})

context("Khiva polyfit3 tests")

test_that("Test polyfit3", {
  ta <- as.single(c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0))
  tb <- as.single(c(0.0, 0.8, 0.9, 0.1,-0.8,-1.0))
  expected <-  c(0.08703704,-0.81349206, 1.69312169,-0.03968254)
  
  a <- Array(array(ta, dim = c(6, 1)))
  b <- Array(array(tb, dim = c(6, 1)))
  
  out <- Polyfit(a, b, 3)
  c <- c(getData(out))
  expect_equal(c, expected, 1e-4)
  deleteArray(a)
  deleteArray(b)
  deleteArray(out)
})

context("Khiva roots tests")

test_that("Test roots", {
  ta <- as.single(c(5,-20, 5, 50,-20,-40))
  expected <-  c(2 + 0i, 2 + 0i, 2 + 0i,-1 + 0i,-1 + 0i)
  
  a <- Array(array(ta, dim = c(6, 1)))
  
  out <- Roots(a)
  b <- c(getData(out))
  expect_equal(b, expected, 1e-2)
  deleteArray(a)
  deleteArray(out)
})
