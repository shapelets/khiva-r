#
#Copyright (c) 2019 Shapelets.io
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

testthat::setup(SetBackend(KHIVABackend()$KHIVA_BACKEND_CPU))

context("Khiva mass tests")

test_that("Test mass", {
  tq <- as.single(c(4, 3, 8))
  q <- Array(array(c(tq), dim = c(3, 1)))
  
  tt <- as.single(c(10, 10, 10, 11, 12, 11, 10, 10, 11, 12, 11, 14, 10, 10))
  t <- Array(array(c(tt), dim = c(14, 1)))
  
  expected.distances <-
    as.double(c(1.732051, 0.328954, 1.210135, 3.150851, 3.245858, 2.822044,
                0.328954, 1.210135, 3.150851, 0.248097, 3.30187, 2.82205))
  out <- Mass(q, t)
  distances <- c(getData(out))
  for (i in 1:length(expected.distances)) {
    expect_equal(distances[i], expected.distances[i], 1e-2)
  }
  deleteArray(t)
  deleteArray(q)
  deleteArray(out)
}) 

test_that("Test mass multiple", {
  tq <- as.double(c(10.0, 10.0, 11.0, 11.0, 10.0, 11.0, 10.0, 10.0))
  q <- Array(array(c(tq), dim = c(4, 2)))
  
  tt <- as.double(c(10.0, 10.0, 10.0, 11.0, 12.0, 11.0, 10.0, 10.0, 11.0, 12.0, 11.0, 14.0, 10.0, 10.0))
  t <- Array(array(c(tt), dim = c(7, 2)))
  
  expected.distances <-
    as.double(c(1.8388, 0.8739, 1.5307, 3.6955, 3.2660, 3.4897, 2.8284, 1.2116,
                1.5307, 2.1758, 2.5783, 3.7550, 2.8284, 2.8284, 3.2159, 0.5020))
  out <- Mass(q, t)
  distances <- c(getData(out))
  for (i in 1:length(expected.distances)) {
    expect_equal(distances[i], expected.distances[i], 1e-2)
  }
  deleteArray(t)
  deleteArray(q)
  deleteArray(out)
})

context("Khiva findBestNOccurrences tests")

test_that("Test findBestNOccurrences", {
  tq <- as.double(c(10.0, 11.0, 12.0))
  q <- Array(array(c(tq), dim = c(3, 1)))
  
  tt <- as.double(c(10.0, 10.0, 11.0, 11.0, 12.0, 11.0, 10.0, 10.0, 11.0, 12.0, 11.0, 10.0, 10.0, 11.0, 
                    10.0, 10.0, 11.0, 11.0, 12.0, 11.0, 10.0, 10.0, 11.0, 12.0, 11.0, 10.0, 10.0, 11.0))
  t <- Array(array(c(tt), dim = c(28, 1)))
  
  out <- FindBestNOccurrences(q, t, 1)
  distances <- c(getData(out$distances))
  indexes <- c(getData(out$indexes))
  
  expect_equal(distances[1], 0, 1e-3)
  expect_equal(indexes[1], 7)
  
  deleteArray(t)
  deleteArray(q)
  deleteArray(out$distances)
  deleteArray(out$indexes)
}) 

test_that("Test findBestNOccurrences multiple", {
  tq <- as.double(c(11.0, 11.0, 10.0, 11.0, 10.0, 11.0, 11.0, 12.0))
  q <- Array(array(c(tq), dim = c(4, 2)))

  tt <- as.double(c(10.0, 10.0, 11.0, 11.0, 10.0, 11.0, 10.0, 10.0, 11.0, 11.0, 10.0, 11.0, 10.0, 10.0,
                    11.0, 10.0, 10.0, 11.0, 10.0, 11.0, 11.0, 10.0, 11.0, 11.0, 14.0, 10.0, 11.0, 10.0))
  t <- Array(array(c(tt), dim = c(14, 2)))

  out <- FindBestNOccurrences(q, t, 4)
  distances <- c(getData(out$distances))
  indexes <- c(getData(out$indexes))

  expect_equal(distances[10], 1.8388, 1e-3)
  expect_equal(indexes[8], 2)

  deleteArray(t)
  deleteArray(q)
  deleteArray(out$distances)
  deleteArray(out$indexes)
}) 

context("Khiva stomp tests")

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

context("Khiva stompSelfJoin tests")

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

context("Khiva findBestNMotifs tests")

test_that("Test findBestNMotifs", {
  ta <-
    as.double(c(10, 10, 10, 10, 10, 10, 9, 10, 10, 10, 10, 10, 11, 10, 9))
  tb <-
    as.double(c(10, 11, 10, 9))
  a <- Array(array(c(ta), dim = c(15, 1)))
  b <- Array(array(c(tb), dim = c(4, 1)))
  
  stomp.results <- Stomp(a, b, 3)
  out <-
    FindBestNMotifs(stomp.results$profile, stomp.results$index, 3, 1, FALSE)
  motif.index <- getData(out$motif.index)
  subsequence.index <- getData(out$subsequence.index)
  expect_equal(motif.index[1], 12, 1e-2)
  expect_equal(subsequence.index[1], 1, 1e-2)
  deleteArray(a)
  deleteArray(b)
  deleteArray(out[[1]])
  deleteArray(out[[2]])
  deleteArray(out[[3]])
})

context("Khiva findBestNMotifsMultipleProfiles tests")

test_that("Test findBestNMotifsMultipleProfiles", {
  ta <-
    as.double(c(10, 10, 10, 10, 10, 10, 9, 10, 10, 10, 10, 10, 11, 10, 9, 10, 10,
                10, 10, 10, 10, 9, 10, 10, 10, 10, 10, 11, 10, 9))
  tb <-
    as.double(c(10, 11, 10, 9, 10, 11, 10, 9))
  a <- Array(array(c(ta), dim = c(15, 2)))
  b <- Array(array(c(tb), dim = c(4, 2)))
  
  stomp.results <- Stomp(a, b, 3)
  out <-
    FindBestNMotifs(stomp.results$profile, stomp.results$index, 3, 1, FALSE)
  motif.index <- getData(out$motif.index)
  subsequence.index <- getData(out$subsequence.index)
  expect_equal(motif.index, array(c(12, 12, 12, 12), dim = c(1, 2, 2, 1)), 1e-2)
  expect_equal(subsequence.index, array(c(1, 1, 1, 1), dim = c(1, 2, 2, 1)), 1e-2)
  deleteArray(a)
  deleteArray(b)
  deleteArray(out[[1]])
  deleteArray(out[[2]])
  deleteArray(out[[3]])
})

context("Khiva findBestNMotifsMirror tests")

test_that("Test findBestNMotifsMirror", {
  ta <-
    as.double(c(10.1, 11, 10.2, 10.15, 10.775, 10.1, 11, 10.2))
  a <- Array(array(c(ta), dim = c(8, 1)))
  
  stomp.results <- StompSelfJoin(a, 3)
  out <-
    FindBestNMotifs(stomp.results$profile, stomp.results$index, 3, 2, TRUE)
  motif.index <- getData(out$motif.index)
  subsequence.index <- getData(out$subsequence.index)
  expect_equal(motif.index, array(c(0, 0), dim = c(2, 1, 1, 1)), 1e-2)
  expect_equal(subsequence.index, array(c(5, 3), dim = c(2, 1, 1, 1)), 1e-2)
  deleteArray(a)
  deleteArray(out[[1]])
  deleteArray(out[[2]])
  deleteArray(out[[3]])
})

context("Khiva findBestNMotifsConsecutive tests")

test_that("Test findBestNMotifsConsecutive", {
  ta <-
    as.double(c(10.1, 11, 10.1, 10.15, 10.075, 10.1, 11, 10.1, 10.15))
  a <- Array(array(c(ta), dim = c(9, 1)))
  
  stomp.results <- StompSelfJoin(a, 3)
  out <-
    FindBestNMotifs(stomp.results$profile, stomp.results$index, 3, 2, TRUE)
  motif.index <- getData(out$motif.index)
  subsequence.index <- getData(out$subsequence.index)
  expect_equal(motif.index[2], 6, 1e-2)
  expect_equal(subsequence.index[2], 3, 1e-2)
  deleteArray(a)
  deleteArray(out[[1]])
  deleteArray(out[[2]])
  deleteArray(out[[3]])
})

context("Khiva findBestNDiscords tests")

test_that("Test findBestNDiscords", {
  ta <-
    as.double(c(11, 10, 11, 10, 11, 10, 11, 10, 11, 10, 11, 10, 1))
  tb <-
    as.double(c(9, 10.1, 10.2, 10.1, 10.2, 10.1, 10.2, 10.1, 10.2, 10.1, 10.2, 10.1, 9))
  a <- Array(array(c(ta), dim = c(13, 1)))
  b <- Array(array(c(tb), dim = c(13, 1)))
  
  stomp.results <- Stomp(a, b, 3)
  out <-
    FindBestNDiscords(stomp.results$profile, stomp.results$index, 3, 2, FALSE)
  subsequence.index <- c(getData(out$subsequence.index))
  expect_equal(subsequence.index[1], 0, 1e-2)
  expect_equal(subsequence.index[2], 10, 1e-2)
  deleteArray(a)
  deleteArray(b)
  deleteArray(out[[1]])
  deleteArray(out[[2]])
  deleteArray(out[[3]])
})

context("Khiva findBestNDiscordsMultipleProfiles tests")

test_that("Test findBestNDiscordsMultipleProfiles", {
  ta <-
    as.double(c(11, 10, 11, 10, 11, 10, 11, 10, 11, 10, 11, 10, 1,
                11, 10, 11, 10, 11, 10, 11, 10, 11, 10, 11, 10, 1))
  tb <-
    as.double(c(9, 10.1, 10.2, 10.1, 10.2, 10.1, 10.2, 10.1, 10.2, 10.1, 10.2, 10.1, 9,
                9, 10.1, 10.2, 10.1, 10.2, 10.1, 10.2, 10.1, 10.2, 10.1, 10.2, 10.1, 9))
  a <- Array(array(c(ta), dim = c(13, 2)))
  b <- Array(array(c(tb), dim = c(13, 2)))
  
  stomp.results <- Stomp(a, b, 3)
  out <-
    FindBestNDiscords(stomp.results$profile, stomp.results$index, 3, 2, FALSE)
  subsequence.index <- getData(out$subsequence.index)
  expect_equal(subsequence.index, array(c(0, 10, 0, 10, 0, 10, 0, 10), dim = c(2, 2, 2, 1)), 1e-2)
  deleteArray(a)
  deleteArray(b)
  deleteArray(out[[1]])
  deleteArray(out[[2]])
  deleteArray(out[[3]])
})

context("Khiva findBestNDiscordsMirror tests")

test_that("Test findBestNDiscordsMirror", {
  ta <-
    as.double(c(10, 11, 10, 10, 11, 10))
  a <- Array(array(c(ta), dim = c(6, 1)))

  stomp.results <- StompSelfJoin(a, 3)
  out <-
    FindBestNDiscords(stomp.results$profile, stomp.results$index, 3, 1, TRUE)
  discord.index <- getData(out$discord.index)
  subsequence.index <- getData(out$subsequence.index)
  expect_equal(discord.index[1], 3, 1e-2)
  expect_equal(subsequence.index[1], 1, 1e-2)
  deleteArray(a)
  deleteArray(out[[1]])
  deleteArray(out[[2]])
  deleteArray(out[[3]])
})

context("Khiva findBestNDiscordsConsecutive tests")

test_that("Test findBestNDiscordsConsecutive", {
  ta <-
    as.double(c(10, 11, 10, 11, 10, 11, 10, 11, 10, 11, 10, 11, 10, 9.999, 9.998))
  a <- Array(array(c(ta), dim = c(15, 1)))
  
  stomp.results <- StompSelfJoin(a, 3)
  out <-
    FindBestNDiscords(stomp.results$profile, stomp.results$index, 3, 2, TRUE)
  subsequence.index <- getData(out$subsequence.index)
  expect_equal(subsequence.index[1], 12, 1e-2)
  if(Sys.getenv(c("TRAVIS")) == "true") {
    expect_equal(subsequence.index[2], 11, 1e-2)
  } else {
    expect_false(isTRUE(all.equal(subsequence.index[2], 11)))
  }
  deleteArray(a)
  deleteArray(out[[1]])
  deleteArray(out[[2]])
  deleteArray(out[[3]])
})
