#
#Copyright (c) 2019 Shapelets.io
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

testthat::setup(
  SetBackend(KHIVABackend()$KHIVA_BACKEND_CPU)
)

context("Khiva Real1D tests")

test_that("Test Real1D", {
  ta <- as.double(c(1, 2, 3, 4, 5, 6, 7, 8))
  expected <- array(ta, dim = c(8, 1, 1, 1))
  a <- Array(expected)
  b <- getData(a)
  expect_equal(b, expected, 1e-4)
  deleteArray(a)
})

context("Khiva Real2D tests")

test_that("Test Real2D", {
  ta <- as.double(c(1, 2, 3, 4, 5, 6, 7, 8))
  expected <- array(ta, dim = c(4, 2, 1, 1))
  a <- Array(expected)
  d <- getData(a)
  expect_equal(d, expected, 1e-4)
  deleteArray(a)
})

context("Khiva Real3D tests")

test_that("Test Real3D", {
  ta <- as.double(c(1, 2, 3, 4, 5, 6, 7, 8))
  expected <- array(ta, dim = c(2, 2, 2, 1))
  a <- Array(expected)
  d <- getData(a)
  expect_equal(d, expected, 1e-4)
  deleteArray(a)
})

context("Khiva Real4D tests")

test_that("Test Real4D", {
  ta <-
    as.double(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
  expected <- array(ta, dim = c(2, 2, 2, 2))
  a <- Array(expected)
  d <- getData(a)
  expect_equal(d, expected, 1e-4)
  deleteArray(a)
})

context("Khiva Complex1D tests")

test_that("Test Complex1D", {
  ta <-
    as.complex(c(1 + 1i, 2 + 2i, 3 + 3i, 4 + 4i, 5 + 5i, 6 + 6i, 7 + 7i, 8 + 8i))
  expected <- array(ta, dim = c(8, 1, 1, 1))
  a <- Array(expected, "c64")
  d <- getData(a)
  expect_equal(d, expected, 1e-4)
  deleteArray(a)
})

context("Khiva Complex2D tests")

test_that("Test Complex2D", {
  ta <-
    as.complex(c(1 + 1i, 2 + 2i, 3 + 3i, 4 + 4i, 5 + 5i, 6 + 6i, 7 + 7i, 8 + 8i))
  expected <- array(ta, dim = c(4, 2, 1, 1))
  a <- Array(expected, "c64")
  d <- getData(a)
  expect_equal(d, expected, 1e-4)
  deleteArray(a)
})

context("Khiva Complex3D tests")

test_that("Test Complex3D", {
  ta <-
    as.complex(c(1 + 1i, 2 + 2i, 3 + 3i, 4 + 4i, 5 + 5i, 6 + 6i, 7 + 7i, 8 + 8i))
  expected <- array(ta, dim = c(2, 2, 2, 1))
  a <- Array(expected, "c64")
  d <- getData(a)
  expect_equal(d, expected, 1e-4)
  deleteArray(a)
})

context("Khiva Complex4D tests")

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

context("Khiva GetType tests")

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

context("Khiva Plus tests")

test_that("Test Plus", {
  ta <- c(1, 2, 3, 4)
  tb <- c(1, 2, 3, 4)
  a <- Array(array(ta, dim = c(4,1,1,1) ))
  b <- Array(array(tb, dim = c(4,1,1,1) ))
  c <- a + b
  expect_equal( c(getData(c)), c(2,4,6,8))
  deleteArray(a)
  deleteArray(b)
  deleteArray(c)

})

context("Khiva Times tests")

test_that("Test Times", {
  ta <- c(1, 2, 3, 4)
  tb <- c(1, 2, 3, 4)
  a <- Array(array(ta, dim = c(4,1,1,1) ))
  b <- Array(array(tb, dim = c(4,1,1,1) ))
  c <- a * b
  expect_equal( c(getData(c)), c(1, 4, 9, 16))
  deleteArray(a)
  deleteArray(b)
  deleteArray(c)
})

context("Khiva Minus tests")

test_that("Test Minus", {
  ta <- c(1, 2, 3, 4)
  tb <- c(1, 2, 3, 4)
  a <- Array(array(ta, dim = c(4,1,1,1) ))
  b <- Array(array(tb, dim = c(4,1,1,1) ))
  c <- a - b
  expect_equal( c(getData(c)), c(0, 0, 0, 0))
  deleteArray(a)
  deleteArray(b)
  deleteArray(c)
})

context("Khiva Divide tests")

test_that("Test Divide", {
  ta <- c(1, 2, 3, 4)
  tb <- c(1, 2, 3, 4)
  a <- Array(array(ta, dim = c(4,1,1,1) ))
  b <- Array(array(tb, dim = c(4,1,1,1) ))
  c <- a / b
  expect_equal( c(getData(c)), c(1, 1, 1, 1), 1e-5)
  deleteArray(a)
  deleteArray(b)
  deleteArray(c)
})

context("Khiva Mod tests")

test_that("Test Mod", {
  ta <- c(1, 2, 3, 4)
  tb <- c(2, 2, 2, 2)
  a <- Array(array(ta, dim = c(4,1,1,1) ))
  b <- Array(array(tb, dim = c(4,1,1,1) ))
  c <- a %% b
  expect_equal( c(getData(c)), c(1, 0, 1, 0), 1e-5)
  deleteArray(a)
  deleteArray(b)
  deleteArray(c)
})

context("Khiva Power tests")

test_that("Test Power", {
  ta <- c(1, 2, 3, 4)
  tb <- c(2, 2, 2, 2)
  a <- Array(array(ta, dim = c(4,1,1,1) ))
  b <- Array(array(tb, dim = c(4,1,1,1) ))
  c <- a ^ b
  expect_equal( c(getData(c)), c(1, 4, 9, 16), 1e-5)
  deleteArray(a)
  deleteArray(b)
  deleteArray(c)
})

context("Khiva Lt tests")

test_that("Test Lt", {
  ta <- c(1, 2, 3, 4)
  tb <- c(2, 2, 2, 2)
  a <- Array(array(ta, dim = c(4,1,1,1) ))
  b <- Array(array(tb, dim = c(4,1,1,1) ))
  c <- a < b
  expect_equal( c(getData(c)), c(TRUE, FALSE, FALSE, FALSE))
  deleteArray(a)
  deleteArray(b)
  deleteArray(c)
})

context("Khiva Gt tests")

test_that("Test Gt", {
  ta <- c(1, 2, 3, 4)
  tb <- c(2, 2, 2, 2)
  a <- Array(array(ta, dim = c(4,1,1,1) ))
  b <- Array(array(tb, dim = c(4,1,1,1) ))
  c <- a > b
  expect_equal( c(getData(c)), c(FALSE, FALSE, TRUE, TRUE))
  deleteArray(a)
  deleteArray(b)
  deleteArray(c)
})

context("Khiva Le tests")

test_that("Test Le", {
  ta <- c(1, 2, 3, 4)
  tb <- c(2, 2, 2, 2)
  a <- Array(array(ta, dim = c(4,1,1,1) ))
  b <- Array(array(tb, dim = c(4,1,1,1) ))
  c <- a <= b
  expect_equal( c(getData(c)), c(TRUE, TRUE, FALSE, FALSE))
  deleteArray(a)
  deleteArray(b)
  deleteArray(c)
})

context("Khiva Ge tests")

test_that("Test Ge", {
  ta <- c(1, 2, 3, 4)
  tb <- c(2, 2, 2, 2)
  a <- Array(array(ta, dim = c(4,1,1,1) ))
  b <- Array(array(tb, dim = c(4,1,1,1) ))
  c <- a >= b
  expect_equal( c(getData(c)), c(FALSE, TRUE, TRUE, TRUE))
  deleteArray(a)
  deleteArray(b)
  deleteArray(c)
})

context("Khiva Eq tests")

test_that("Test Eq", {
  ta <- c(1, 2, 3, 4)
  tb <- c(1, 2, 3, 5)
  a <- Array(array(ta, dim = c(4,1,1,1) ))
  b <- Array(array(tb, dim = c(4,1,1,1) ))
  c <- a == b
  expect_equal( c(getData(c)), c(TRUE, TRUE, TRUE, FALSE))
  deleteArray(a)
  deleteArray(b)
  deleteArray(c)
})

context("Khiva Neq tests")

test_that("Test Neq", {
  ta <- c(1, 2, 3, 4)
  tb <- c(1, 2, 3, 5)
  a <- Array(array(ta, dim = c(4,1,1,1) ))
  b <- Array(array(tb, dim = c(4,1,1,1) ))
  c <- a != b
  expect_equal( c(getData(c)), c(FALSE, FALSE, FALSE, TRUE))
  deleteArray(a)
  deleteArray(b)
  deleteArray(c)
})

context("Khiva And tests")

test_that("Test And", {
  ta <- c(TRUE, TRUE, TRUE, TRUE)
  tb <- c(TRUE, FALSE, TRUE, FALSE)
  a <- Array(array(ta, dim = c(4,1,1,1) ), "b8")
  b <- Array(array(tb, dim = c(4,1,1,1) ), "b8")
  c <- a & b
  expect_equal( c(getData(c)), c(TRUE, FALSE, TRUE, FALSE))
  deleteArray(a)
  deleteArray(b)
  deleteArray(c)
})

context("Khiva Or tests")

test_that("Test Or", {
  ta <- c(TRUE, TRUE, TRUE, TRUE)
  tb <- c(TRUE, FALSE, TRUE, FALSE)
  a <- Array(array(ta, dim = c(4,1,1,1) ), "b8")
  b <- Array(array(tb, dim = c(4,1,1,1) ), "b8")
  c <- a | b
  expect_equal( c(getData(c)), c(TRUE, TRUE, TRUE, TRUE))
  deleteArray(a)
  deleteArray(b)
  deleteArray(c)
})

context("Khiva Xor tests")

test_that("Test Xor", {
  ta <- c(TRUE, TRUE, TRUE, TRUE)
  tb <- c(TRUE, FALSE, TRUE, FALSE)
  a <- Array(array(ta, dim = c(4,1,1,1) ), "b8")
  b <- Array(array(tb, dim = c(4,1,1,1) ), "b8")
  c <- xor.khiva(a,b)
  expect_equal( c(getData(c)), c(FALSE, TRUE, FALSE, TRUE))
  deleteArray(a)
  deleteArray(b)
  deleteArray(c)
})

context("Khiva Bitshift tests")

test_that("Test Bitshift", {
  ta <- as.integer(c(2, 4, 6, 8))
  a <- Array(array(ta, dim = c(4,1,1,1) ), "s32")
  c <- bitShiftR(a, 1)
  expect_equal( c(getData(c)), c(1, 2, 3, 4))
  deleteArray(a)
  deleteArray(c)
})

context("Khiva Bitsra tests")

test_that("Test Bitsra", {
  ta <- as.integer(c(2, 4, 6, 8))
  a <- Array(array(ta, dim = c(4,1,1,1) ), "s32")
  c <- bitShiftL(a, 1)
  expect_equal( c(getData(c)), c(4, 8, 12, 16))
  deleteArray(a)
  deleteArray(c)
})

context("Khiva Ctranspose tests")

test_that("Test CTranspose", {
  ta = as.complex(c(0-1i, 4 +2i, 2 +1i, 0 -2i))
  a <- Array(array(ta, dim= c(2, 2, 1, 1)), "c32")
  c <- transpose(a, TRUE)
  expect_equal( c(getData(c)), c(0 +1i, 2-1i, 4 -2i, 0 + 2i))
  deleteArray(a)
  deleteArray(c)
})

context("Khiva Col tests")

test_that("Test Col", {
  a <- Array(array(c(1, 3, 2 ,4), dim=c(2,2)))
  c <- getCol(a, 0)
  expect_equal( c(getData(c)), c(1, 3))
  deleteArray(a)
  deleteArray(c)

})

context("Khiva Cols tests")

test_that("Test Cols", {
  a <- Array(array(c(1, 4, 2, 5, 3, 6), dim=c(2,3)))
  c <- getCols(a, 0, 1)
  expect_equal( c(getData(c)), c(1, 4, 2, 5))
  deleteArray(a)
  deleteArray(c)
})

context("Khiva Row tests")

test_that("Test Row", {
  a <- Array(array(c(1, 3, 2, 4), dim=c(2,2)))
  c <- getRow(a, 0)
  expect_equal( c(getData(c)), c(1, 2))
  deleteArray(a)
  deleteArray(c)
})

context("Khiva Rows tests")

test_that("Test Rows", {
  a <- Array(array(c(1, 4, 2, 5, 3, 6), dim=c(3, 2)))
  c <- getRows(a, 0, 1)
  expect_equal( c(getData(c)), c(1, 4, 5, 3))
  deleteArray(a)
  deleteArray(c)
})

context("Khiva Mtimes tests")

test_that(" Test Mtimes", {
  a <- Array(array(c(1, 2, 3, 4), dim = c(4, 1, 1, 1)))
  b <- Array(array(c(1, 2, 3, 4), dim = c(1, 4, 1, 1)))
  c <- matMult(a, b)
  expect_equal( c(getData(c)), c(1, 2, 3, 4, 2, 4, 6, 8, 3, 6, 9, 12, 4, 8, 12, 16))
  deleteArray(a)
  deleteArray(b)
  deleteArray(c)
})

context("Khiva As tests")

test_that("Test As", {
  a <- Array(array(as.integer(c(1, 2, 3, 4)), dim = c(4, 1, 1, 1)), "s32")
  b <- asType(a, "f32")
  expect_equal( c(getData(b)), c(1, 2, 3, 4))
  expect_equal(getType(b), 0)
  deleteArray(a)
  deleteArray(b)
})

context("Khiva Copy tests")

test_that("Test Copy", {
  a <- Array(array(c(1.1, 2.1, 3.1, 4.1), dim = c(4, 1, 1, 1)))
  b <- copy(a)
  expect_equal( c(getData(b)), c(getData(a)))
  expect_equal(getType(b), getType(a))
  deleteArray(a)
  deleteArray(b)
})

