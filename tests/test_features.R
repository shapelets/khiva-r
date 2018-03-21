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

test_that("Test crossCorrelation", {
  ta <- as.double(c(1, 2, 3, 4))
  tb <- as.double(c(4, 6, 8, 10, 12))
  out <- CrossCorrelation(list(ta), list(tb), FALSE)
  
  expect_equal(out$result[1], 0.790569415)
  expect_equal(out$result[2], 0.790569415)
  expect_equal(out$result[3], 0.079056941)
  expect_equal(out$result[4],-0.395284707)
  expect_equal(out$result[5],-0.474341649)
})

test_that("Test autoCovariance", {
  ta <- as.double(c(0, 1, 2, 3))
  tb <- as.double(c(10, 11, 12, 13))
  out <- AutoCovariance(list(ta, tb), FALSE)
  
  expect_equal(out$result[1], 1.25)
  expect_equal(out$result[2], 0.3125)
  expect_equal(out$result[3],-0.375)
  expect_equal(out$result[4],-0.5625)
  expect_equal(out$result[5], 1.25)
  expect_equal(out$result[6], 0.3125)
  expect_equal(out$result[7],-0.375)
  expect_equal(out$result[8],-0.5625)
})

test_that("Test CrossCovariance", {
  ta <- as.double(c(0, 1, 2, 3))
  tb <- as.double(c(10, 11, 12, 13))
  tc <- as.double(c(4, 6, 8, 10, 12))
  td <- as.double(c(14, 16, 18, 20, 22))
  out <- CrossCovariance(list(ta, tb), list(tc, td), FALSE)
  
  for (i in 0:3) {
    expect_equal(out$result[(i * 5) + 1], 2.5)
    expect_equal(out$result[(i * 5) + 2], 2.5)
    expect_equal(out$result[(i * 5) + 3], 0.25)
    expect_equal(out$result[(i * 5) + 4],-1.25)
    expect_equal(out$result[(i * 5) + 5],-1.5)
  }
})

test_that("Test ApproximateEntropy", {
  ta <- as.double(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  tb <- as.double(c(11, 12, 13, 14, 15, 16, 17, 18, 19, 20))
  
  out <- ApproximateEntropy(list(ta, tb), 4, 0.5)
  expect_equal(out$result[1], 0.13484275341033936, 1e-6)
  expect_equal(out$result[2], 0.13484275341033936, 1e-6)
})
