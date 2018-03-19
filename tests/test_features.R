#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

test_that("Test c3", {
  ta <- as.double(c(0, 1, 2, 3, 4, 5))
  tb <- as.double(c(6, 7, 8, 9, 10, 11))
  out <- C3((list(ta, tb)), 2)
  
  expect_equal(out$result[1], 7.5)
  expect_equal(out$result[2], 586.5)
})

test_that("Test cidCe", {
  ta <- as.double(c(0, 1, 2, 3, 4, 5))
  tb <- as.double(c(6, 7, 8, 9, 10, 11))
  out <- CidCe((list(ta, tb)), TRUE)
  
  expect_equal(out$result[1], 1.30930734141595)
  expect_equal(out$result[2], 1.30930734141595)
  
  out <- CidCe((list(ta, tb)), FALSE)
  expect_equal(out$result[1], 2.23606797749979)
  expect_equal(out$result[2], 2.23606797749979)
})

test_that("Test absEnergy", {
  ta <- as.double(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  out <- AbsEnergy(list(ta))
  
  expect_equal(out$result[1], 385)
})

test_that("Test absoluteSumOfChanges", {
  ta <- as.double(c(0, 1, 2, 3))
  tb <- as.double(c(4, 6, 8, 10))
  tc <- as.double(c(11, 14, 17, 20))
  out <- AbsoluteSumOfChanges(list(ta, tb, tc))
  
  expect_equal(out$result[1], 3)
  expect_equal(out$result[2], 6)
  expect_equal(out$result[3], 9)
})