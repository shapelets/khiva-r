#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

test_that("Test Real1D", {
  ta <- as.double(c(1, 2, 3, 4, 5, 6, 7, 8))
  expected <- array(ta, dim = c(8, 1, 1, 1))
  a <- Array(expected)
  d <- getData(a)
  expect_equal(d, expected, 1e-4)
  deleteArray(a)
})

test_that("Test Real2D", {
  ta <- as.double(c(1, 2, 3, 4, 5, 6, 7, 8))
  expected <- array(ta, dim = c(4, 2, 1, 1))
  a <- Array(expected)
  d <- getData(a)
  expect_equal(d, expected, 1e-4)
  deleteArray(a)
})

test_that("Test Real3D", {
  ta <- as.double(c(1, 2, 3, 4, 5, 6, 7, 8))
  expected <- array(ta, dim = c(2, 2, 2, 1))
  a <- Array(expected)
  d <- getData(a)
  expect_equal(d, expected, 1e-4)
  deleteArray(a)
})

test_that("Test Real4D", {
  ta <-
    as.double(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
  expected <- array(ta, dim = c(2, 2, 2, 2))
  a <- Array(expected)
  d <- getData(a)
  expect_equal(d, expected, 1e-4)
  deleteArray(a)
})

test_that("Test Complex1D", {
  ta <-
    as.complex(c(1 + 1i, 2 + 2i, 3 + 3i, 4 + 4i, 5 + 5i, 6 + 6i, 7 + 7i, 8 + 8i))
  expected <- array(ta, dim = c(8, 1, 1, 1))
  a <- Array(expected, "c64")
  d <- getData(a)
  expect_equal(d, expected, 1e-4)
  deleteArray(a)
})

test_that("Test Complex2D", {
  ta <-
    as.complex(c(1 + 1i, 2 + 2i, 3 + 3i, 4 + 4i, 5 + 5i, 6 + 6i, 7 + 7i, 8 + 8i))
  expected <- array(ta, dim = c(4, 2, 1, 1))
  a <- Array(expected, "c64")
  d <- getData(a)
  expect_equal(d, expected, 1e-4)
  deleteArray(a)
})

test_that("Test Complex3D", {
  ta <-
    as.complex(c(1 + 1i, 2 + 2i, 3 + 3i, 4 + 4i, 5 + 5i, 6 + 6i, 7 + 7i, 8 + 8i))
  expected <- array(ta, dim = c(2, 2, 2, 1))
  a <- Array(expected, "c64")
  d <- getData(a)
  expect_equal(d, expected, 1e-4)
  deleteArray(a)
})

test_that("Test Complex4D", {
  ta <-
    as.complex(
      c(
        1 + 1i,
        2 + 2i,
        3 + 3i,
        4 + 4i,
        5 + 5i,
        6 + 6i,
        7 + 7i,
        8 + 8i,
        9 + 9i,
        10 + 10i,
        11 + 11i,
        12 + 12i,
        13 + 13i,
        14 + 14i,
        15 + 15i,
        16 + 16i
      )
    )
  expected <- array(ta, dim = c(2, 2, 2, 2))
  a <- Array(expected, "c64")
  d <- getData(a)
  expect_equal(d, expected, 1e-4)
  deleteArray(a)
})

test_that("Test GetType", {
  ta <-
    as.complex(
      c(
        1 + 1i,
        2 + 2i,
        3 + 3i,
        4 + 4i,
        5 + 5i,
        6 + 6i,
        7 + 7i,
        8 + 8i,
        9 + 9i,
        10 + 10i,
        11 + 11i,
        12 + 12i,
        13 + 13i,
        14 + 14i,
        15 + 15i,
        16 + 16i
      )
    )
  expected <- array(ta, dim = c(2, 2, 2, 2))
  a <- Array(expected, "c64")
  d <- getType(a)
  e <- getTypeName(d)
  expect_equal(e, "c64")
  deleteArray(a)
})