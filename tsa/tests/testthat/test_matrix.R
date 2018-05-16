#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

test_that("Test stomp", {
  ta <- as.single(c(10, 11, 10, 11))
  tb <- as.single(c(10, 11, 10, 11))
  tc <- as.single(c(10, 11, 10, 11, 10, 11, 10, 11))
  td <- as.single(c(10, 11, 10, 11, 10, 11, 10, 11))
  a <- Array(array(c(ta, tb), dim = c(4, 2)))
  b <- Array(array(c(tc, td), dim = c(8, 2)))
  expected.index <-
    as.integer(c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1))
  out <- Stomp(a, b, 3)
  profile <- c(getData(out$profile))
  index <- c(getData(out$index))
  for (i in 1:length(expected.index)) {
    expect_equal(profile[i], 0, 1e-2)
    expect_equal(index[i], expected.index[i], 1e-2)
  }
  deleteArray(a)
  deleteArray(b)
  deleteArray(out[[1]])
  deleteArray(out[[2]])
})

test_that("Test stompSelfJoin", {
  ta <-
    as.double(c(10, 10, 11, 11, 10, 11, 10, 10, 11, 11, 10, 11, 10, 10))
  tb <-
    as.double(c(11, 10, 10, 11, 10, 11, 11, 10, 11, 11, 10, 10, 11, 10))
  expected.index <-
    as.integer(c(6, 7, 8, 9, 10, 11, 0, 1, 2, 3, 4, 5, 9, 10, 11, 6, 7, 8, 3, 4, 5, 0, 1, 2))
  a <- Array(array(c(ta, tb), dim = c(14, 2)))
  out <- StompSelfJoin(a, 3)
  profile <- getData(out$profile)
  index <- getData(out$index)
  for (i in 1:length(expected.index)) {
    expect_equal(profile[i], 0, 2e-2)
    expect_equal(index[i], expected.index[i], 1e-2)
  }
  deleteArray(a)
  deleteArray(out[[1]])
  deleteArray(out[[2]])
  
})

test_that("Test findBestNMotifs", {
  ta <-
    as.double(c(10, 10, 10, 10, 10, 10, 9, 10, 10, 10, 10, 10, 11, 10, 9))
  tb <-
    as.double(c(10, 11, 10, 9))
  a <- Array(array(c(ta), dim = c(15, 1)))
  b <- Array(array(c(tb), dim = c(4, 1)))
  
  stomp.results <- Stomp(a, b, 3)
  out <-
    FindBestNMotifs(stomp.results$profile, stomp.results$index, 2)
  motif.index <- getData(out$motif.index)
  subsequence.index <- getData(out$subsequence.index)
  expect_equal(motif.index[1], 12, 1e-2)
  expect_equal(motif.index[2], 11, 1e-2)
  expect_equal(subsequence.index[1], 1, 1e-2)
  expect_equal(subsequence.index[2], 0, 1e-2)
  deleteArray(a)
  deleteArray(b)
  deleteArray(out[[1]])
  deleteArray(out[[2]])
  deleteArray(out[[3]])
})

test_that("Test findBestNDiscords", {
  ta <-
    as.double(c(11, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11))
  tb <-
    as.double(c(9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 9))
  a <- Array(array(c(ta), dim = c(12, 1)))
  b <- Array(array(c(tb), dim = c(12, 1)))
  
  stomp.results <- Stomp(a, b, 3)
  out <-
    FindBestNDiscords(stomp.results$profile, stomp.results$index, 2)
  subsequence.index <- c(getData(out$subsequence.index))
  expect_equal(subsequence.index[1], 0, 1e-2)
  expect_equal(subsequence.index[2], 9, 1e-2)
  deleteArray(a)
  deleteArray(b)
  deleteArray(out[[1]])
  deleteArray(out[[2]])
  #deleteArray(out[[3]])
  
})