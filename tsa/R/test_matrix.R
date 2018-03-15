#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.


test_that("Test stomp", {
  ta <- as.double(c(10, 10, 10, 11, 12, 11, 10, 10, 11, 12, 11, 10, 10, 10))
  tb <- as.double(c(10, 10, 10, 11, 12, 11, 10, 10, 11, 12, 11, 10, 10, 10))
  expected.index <- as.integer(c(11, 1, 2, 8, 9, 10, 1, 2, 8, 9, 10, 11))
  out <- Stomp(ta, tb, 3)
  
  for (i in 1:length(expected.index)) {
    expect_equal(out$profile[i], 0, 1e-6)
    expect_equal(out$index[i], expected.index[i], 1e-6)
  }
})

test_that("Test stompSelJoin", {
  ta <- as.double(c(10, 10, 10, 11, 12, 11, 10, 10, 11, 12, 11, 10, 10, 10))
  expected.index <- as.integer(c(11, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 0))
  out <- StompSelfJoin(ta, 3)
  
  for (i in 1:length(expected.index)) {
    expect_equal(out$profile[i], 0, 1e-6)
    expect_equal(out$index[i], expected.index[i], 1e-6)
  }
})

test_that("Test findBestNMotifs", {
  ta <- as.double(c(10, 11, 10, 10, 10, 10, 9, 10, 10, 10, 10, 10, 11, 10))
  tb <- as.double(c(10, 11, 10, 300, 20, 30, 40, 50, 60, 70, 80, 90, 80, 90))
  stomp.results <- Stomp(ta, tb, 3)
  out <- FindBestNMotifs(stomp.results$profile, stomp.results$index, 3)
  
  expect_equal(out$motif.index[1], 0)
  expect_equal(out$motif.index[2], 0)
  expect_equal(out$subsequence.index[1], 0)
  expect_equal(a$subsequence.index[2], 10)
})

test_that("Test findBestNDiscords", {
  ta <- as.double(c(10, 11, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 10))
  tb <- as.double(c(10, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 9, 10))
  stomp.results <- Stomp(ta, tb, 3)
  out <- FindBestNDiscords(stomp.results$profile, stomp.results$index, 3)
  
  expect_equal(out$subsequence.index[1], 0)
  expect_equal(out$subsequence.index[2], 11)
})