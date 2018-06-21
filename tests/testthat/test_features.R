#
#Copyright (c) 2018 Shapelets.io
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

testthat::setup(
  SetBackend(KHIVABackend()$KHIVA_BACKEND_CPU)
)

context("Khiva C3 tests")

test_that("Test C3", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5))
  tb <- as.single(c(6, 7, 8, 9, 10, 11))
  a <- Array(array(c(ta, tb), dim = c(6, 2)))
  out <- C3(a, 2)
  b <- c(getData(out))
  expect_equal(b[1], 7.5, 1e-6)
  expect_equal(b[2], 586.5, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva CidCe tests")

test_that("Test CidCe", {
  # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <- as.single(c(0, 1, 2, 3, 4, 5))
    tb <- as.single(c(6, 7, 8, 9, 10, 11))
    a <- Array(array(c(ta, tb), dim = c(6, 2)))
    out <- CidCe(a, TRUE)
    b <- c(getData(out))
    expect_equal(b[1], 1.30930734141595, 1e-6)
    expect_equal(b[2], 1.30930734141595, 1e-6)

    out <- CidCe(a, FALSE)
    b <- c(getData(out))
    expect_equal(b[1], 2.23606797749979, 1e-6)
    expect_equal(b[2], 2.23606797749979, 1e-6)
    deleteArray(a)
    deleteArray(out)
  }
})

context("Khiva AbsEnergy tests")

test_that("Test AbsEnergy", {
  # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
    if (travis.platform != 'osx') {
    ta <- as.single(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    a <- Array(array(ta, dim = c(10, 1)))
    out <- AbsEnergy(a)
    b <- c(getData(out))
    expect_equal(b[1], 385, 1e-6)
    deleteArray(a)
    deleteArray(out)
  }
})

context("Khiva AbsoluteSumOfChanges tests")

test_that("Test AbsoluteSumOfChanges", {
  ta <- as.single(c(0, 1, 2, 3))
  tb <- as.single(c(4, 6, 8, 10))
  tc <- as.single(c(11, 14, 17, 20))
  a <- Array(array(c(ta, tb, tc), dim = c(4, 3)))
  out <- AbsoluteSumOfChanges(a)
  b <- c(getData(out))
  expect_equal(b[1], 3, 1e-6)
  expect_equal(b[2], 6, 1e-6)
  expect_equal(b[3], 9, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva CrossCorrelation tests")

test_that("Test CrossCorrelation", {
  # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <- as.single(c(1, 2, 3, 4))
    tb <- as.single(c(4, 6, 8, 10, 12))
    a <- Array(array(ta, dim = c(4, 1)))
    b <- Array(array(tb, dim = c(5, 1)))
    out <- CrossCorrelation(a, b, FALSE)
    c <- c(getData(out))
    expect_equal(c[1], 0.790569415, 1e-6)
    expect_equal(c[2], 0.790569415, 1e-6)
    expect_equal(c[3], 0.079056941, 1e-6)
    expect_equal(c[4], -0.395284707, 1e-6)
    expect_equal(c[5], -0.474341649, 1e-6)
    deleteArray(a)
    deleteArray(out)
    deleteArray(b)
  }
})

context("Khiva AutoCovariance tests")

test_that("Test AutoCovariance", {
  # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <- as.single(c(0, 1, 2, 3))
    tb <- as.single(c(10, 11, 12, 13))
    a <- Array(array(c(ta, tb), dim = c(4, 2)))
    out <- AutoCovariance(a, FALSE)
    b <- c(getData(out))
    expect_equal(b[1], 1.25, 1e-6)
    expect_equal(b[2], 0.3125, 1e-6)
    expect_equal(b[3], -0.375, 1e-6)
    expect_equal(b[4], -0.5625, 1e-6)
    expect_equal(b[5], 1.25, 1e-6)
    expect_equal(b[6], 0.3125, 1e-6)
    expect_equal(b[7], -0.375, 1e-6)
    expect_equal(b[8], -0.5625, 1e-6)
    deleteArray(out)
    deleteArray(a)
  }
})

context("Khiva CrossCovariance tests")

test_that("Test CrossCovariance", {
   # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <- as.single(c(0, 1, 2, 3))
    tb <- as.single(c(10, 11, 12, 13))
    tc <- as.single(c(4, 6, 8, 10, 12))
    td <- as.single(c(14, 16, 18, 20, 22))
    a <- Array(array(c(ta, tb), dim = c(4, 2)))
    b <- Array(array(c(tc, td), dim = c(5, 2)))

    out <- CrossCovariance(a, b, FALSE)

    c <- c(getData(out))
    for (i in 0:3) {
      expect_equal(c[(i * 5) + 1], 2.5, 1e-6)
      expect_equal(c[(i * 5) + 2], 2.5, 1e-6)
      expect_equal(c[(i * 5) + 3], 0.25, 1e-6)
      expect_equal(c[(i * 5) + 4], -1.25, 1e-6)
      expect_equal(c[(i * 5) + 5], -1.5, 1e-6)
    }
    deleteArray(b)
    deleteArray(a)
    deleteArray(out)
  }
})

context("Khiva ApproximateEntropy tests")

test_that("Test ApproximateEntropy", {
  # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <- as.single(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    a <- Array(array(ta, dim = c(10, 2)))
    out <- ApproximateEntropy(a, 4, 0.5)
    b <- c(getData(out))
    expect_equal(b[1], 0.13484275341033936, 1e-6)
    expect_equal(b[2], 0.13484275341033936, 1e-6)
    deleteArray(a)
    deleteArray(out)
  }
})

context("Khiva AutoCorrelation tests")

test_that("Test AutoCorrelation", {
  # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <- as.single(c(0, 1, 2, 3))
    tb <- as.single(c(10, 11, 12, 13))

    a <- Array(array(c(ta, tb), dim = c(4, 2)))
    out <- AutoCorrelation(a, 4, FALSE)
    c <- c(getData(out))
    expect_equal(c[1], 1, 1e-6)
    expect_equal(c[2], 0.25, 1e-6)
    expect_equal(c[3],-0.3, 1e-6)
    expect_equal(c[4],-0.45, 1e-6)
    expect_equal(c[5], 1, 1e-6)
    expect_equal(c[6], 0.25, 1e-6)
    expect_equal(c[7],-0.3, 1e-6)
    expect_equal(c[8],-0.45, 1e-6)
    deleteArray(a)
    deleteArray(out)
  }
})

context("Khiva BinnedEntropy tests")

test_that("Test BinnedEntropy", {
   # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <- as.single(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
                      14, 15, 16, 17, 18, 19, 20))
    tb <-
      as.single(c(1, 1, 3, 10, 5, 6, 1, 8, 9, 10, 11, 1, 13, 14, 10, 16, 17, 10, 19,
                  20))
    a <- Array(array(c(ta, tb), dim = c(20, 2)))

    out <- BinnedEntropy(a, 5)
    c <- c(getData(out))
    expect_equal(c[1], 1.6094379124341005, 1e-6)
    expect_equal(c[2], 1.5614694247763998, 1e-6)
    deleteArray(a)
    deleteArray(out)
  }
})

context("Khiva CountAboveMean tests")

test_that("Test CountAboveMean", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5))
  tb <- as.single(c(6, 7, 8, 9, 10, 11))
  a <- Array(array(c(ta, tb), dim = c(6, 2)))
  
  out <- CountAboveMean(a)
  
  c <- c(getData(out))
  expect_equal(c[1], 3, 1e-6)
  expect_equal(c[2], 3, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva CountBelowMean tests")

test_that("Test CountBelowMean", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5))
  tb <- as.single(c(6, 7, 8, 9, 10, 11))
  a <- Array(array(c(ta, tb), dim = c(6, 2)))
  out <- CountBelowMean(a)
  b <- c(getData(out))
  expect_equal(b[1], 3, 1e-6)
  expect_equal(b[2], 3, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva EnergyRatioByChunks tests")

test_that("Test EnergyRatioBychunks", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5))
  tb <- as.single(c(6, 7, 8, 9, 10, 11))
  
  a <- Array(array(c(ta, tb), dim = c(6, 2)))
  out <- EnergyRatioByChunks(a, 2, 0)
  b <- c(getData(out))
  expect_equal(b[1], 0.090909091, 1e-6)
  expect_equal(b[2], 0.330376940, 1e-6)
  
  out <- EnergyRatioByChunks(a, 2, 1)
  b <- c(getData(out))
  deleteArray(a)
  deleteArray(out)
  
  expect_equal(b[1], 0.909090909, 1e-6)
  expect_equal(b[2], 0.669623060, 1e-6)
})

context("Khiva CountBelowMean tests")

test_that("Test CountBelowMean", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5))
  tb <- as.single(c(6, 7, 8, 9, 10, 11))
  a <- Array(array(c(ta, tb), dim = c(6, 2)))
  out <- CountBelowMean(a)
  b <- c(getData(out))
  expect_equal(b[1], 3, 1e-6)
  expect_equal(b[2], 3, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva FirstLocationOfMaximum tests")

test_that("Test FirstLocationOfMaximum", {
  ta <- as.single(c(5, 4, 3, 5, 0, 1, 5, 3, 2, 1))
  tb <- as.single(c(2, 4, 3, 5, 2, 5, 4, 3, 5, 2))
  a <- Array(array(c(ta, tb), dim = c(10, 2)))
  out <- FirstLocationOfMaximum(a)
  c <- c(getData(out))
  expect_equal(c[1], 0.0, 1e-6)
  expect_equal(c[2], 0.3, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva FirstLocationOfMinimum tests")

test_that("Test FirstLocationOfMinimum", {
  ta <- as.single(c(5, 4, 3, 0, 0, 1))
  tb <- as.single(c(5, 4, 3, 0, 2, 1))
  a <- Array(array(c(ta, tb), dim = c(6, 2)))
  out <- FirstLocationOfMinimum(a)
  c <- c(getData(out))
  expect_equal(c[1], 0.5, 1e-6)
  expect_equal(c[2], 0.5, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva HasDuplicates tests")

test_that("Test HasDuplicates", {
  ta <- as.single(c(5, 4, 3, 0, 0, 1))
  tb <- as.single(c(5, 4, 3, 0, 2, 1))
  a <- Array(array(c(ta, tb), dim = c(6, 2)))
  
  out <- HasDuplicates(a)
  c <- c(getData(out))
  
  expect_equal(c[1], TRUE)
  expect_equal(c[2], FALSE)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva HasDuplicateMax tests")

test_that("Test HasDuplicateMax", {
  ta <- as.single(c(5, 4, 3, 0, 5, 1))
  tb <- as.single(c(5, 4, 3, 0, 2, 1))
  a <- Array(array(c(ta, tb), dim = c(6, 2)))
  
  out <- HasDuplicateMax(a)
  c <- c(getData(out))
  
  expect_equal(c[1], TRUE)
  expect_equal(c[2], FALSE)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva IndexMassQuantile tests")

test_that("Test IndexMassQuantile", {
  ta <- as.single(c(5, 4, 3, 0, 5, 1))
  tb <- as.single(c(5, 4, 3, 0, 2, 1))
  a <- Array(array(c(ta, tb), dim = c(6, 2)))
  
  out <- IndexMassQuantile(a, 0.5)
  c <- c(getData(out))
  
  expect_equal(c[1], 0.333333333, 1e-6)
  expect_equal(c[2], 0.333333333, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva Kurtosis tests")

test_that("Test Kurtosis", {
   # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <- as.single(c(0, 1, 2, 3, 4, 5))
    tb <- as.single(c(2, 2, 2, 20, 30, 25))
    a <- Array(array(c(ta, tb), dim = c(6, 2)))

    out <- Kurtosis(a)
    c <- c(getData(out))

    expect_equal(c[1], -1.2, 1e-6)
    expect_equal(c[2], -2.66226722, 1e-6)
    deleteArray(a)
    deleteArray(out)
  }
})

context("Khiva LargeStandardDeviation tests")

test_that("Test LargeStandardDeviation", {
  ta <- as.single(c(-1, -1, -1, 1, 1, 1))
  tb <- as.single(c(4, 6, 8, 4, 5, 4))
  a <- Array(array(c(ta, tb), dim = c(6, 2)))
  
  out <- LargeStandardDeviation(a, 0.4)
  c <- c(getData(out))
  
  expect_equal(c[1], TRUE)
  expect_equal(c[2], FALSE)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva LastLocationOfMaximum tests")

test_that("Test LastLocationOfMaximum", {
  ta <- as.single(c(0, 4, 3, 5, 5, 1))
  tb <- as.single(c(0, 4, 3, 2, 5, 1))
  a <- Array(array(c(ta, tb), dim = c(6, 2)))
  out <- LastLocationOfMaximum(a)
  c <- c(getData(out))
  deleteArray(a)
  deleteArray(out)
  expect_equal(c[1], 0.8333333333333334, 1e-6)
  expect_equal(c[2], 0.8333333333333334, 1e-6)
})

context("Khiva LastLocationOfMinimum tests")

test_that("Test LastLocationOfMinimum", {
  ta <- as.single(c(0, 4, 3, 5, 5, 1, 0, 4))
  tb <- as.single(c(3, 2, 5, 1, 4, 5, 1, 2))
  a <- Array(array(c(ta, tb), dim = c(8, 2)))
  
  out <- LastLocationOfMinimum(a)
  c <- c(getData(out))
  deleteArray(a)
  deleteArray(out)
  
  expect_equal(c[1], 0.875, 1e-6)
  expect_equal(c[2], 0.875, 1e-6)
})

context("Khiva Length tests")

test_that("Test Length", {
  ta <- as.single(c(0, 4, 3, 5, 5, 1))
  tb <- as.single(c(0, 4, 3, 2, 5, 1))
  a <- Array(array(c(ta, tb), dim = c(6, 2)))
  
  out <- Length(a)
  c <- c(getData(out))
  deleteArray(a)
  deleteArray(out)
  
  expect_equal(c[1], 6, 1e-6)
  expect_equal(c[2], 6, 1e-6)
})

context("Khiva LinearTrend tests")

test_that("Test LinearTrend", {
  ta <- as.single(c(0, 4, 3, 5, 5, 1))
  tb <- as.single(c(2, 4, 1, 2, 5, 3))
  a <- Array(array(c(ta, tb), dim = c(6, 2)))
  out <- LinearTrend(a)
  pvalue <- c(getData(out$pvalue))
  rvalue <- c(getData(out$rvalue))
  intercept <- c(getData(out$intercept))
  slope <- c(getData(out$slope))
  stdrr <- c(getData(out$stdrr))
  expect_equal(pvalue[1], 0.6260380997892747, 1e-6)
  expect_equal(pvalue[2], 0.5272201945463578, 1e-6)
  
  expect_equal(rvalue[1], 0.2548235957188128, 1e-6)
  expect_equal(rvalue[2], 0.3268228676411533, 1e-6)
  
  expect_equal(intercept[1], 2.2857142857142856, 1e-6)
  expect_equal(intercept[2], 2.1904761904761907, 1e-6)
  
  expect_equal(slope[1], 0.2857142857142857, 1e-6)
  expect_equal(slope[2], 0.2571428571428572, 1e-6)
  
  expect_equal(stdrr[1], 0.5421047417431507, 1e-6)
  expect_equal(stdrr[2], 0.37179469135129783, 1e-6)
  deleteArray(a)
  deleteArray(out$pvalue)
  deleteArray(out$rvalue)
  deleteArray(out$intercept)
  deleteArray(out$slope)
  deleteArray(out$stdrr)
})

context("Khiva HasDuplicateMin tests")

test_that("Test HasDuplicateMin", {
  ta <- as.single(c(5, 4, 3, 0, 0, 1))
  tb <- as.single(c(5, 4, 3, 0, 2, 1))
  a <- Array(array(c(ta, tb), dim = c(6, 2)))
  out <- HasDuplicateMin(a)
  b <- c(getData(out))
  expect_equal(b[1], TRUE)
  expect_equal(b[2], FALSE)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva LongestStrikeAboveMean tests")

test_that("Test LongestStrikeAboveMean", {
  ta <-
    as.single(c(20, 20, 20, 1, 1, 1, 20, 20, 20, 20, 1, 1, 1, 1, 1, 1, 1, 1, 20, 20))
  tb <-
    as.single(c(20, 20, 20, 1, 1, 1, 20, 20, 20, 1,  1, 1, 1, 1, 1, 1, 1, 1, 20, 20))
  a <- Array(array(c(ta, tb), dim = c(20, 2)))
  out <- LongestStrikeAboveMean(a)
  b <- c(getData(out))
  expect_equal(b[1], 4, 1e-6)
  expect_equal(b[2], 3, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva LongestStrikeBelowMean tests")

test_that("Test LongestStrikeBelowMean", {
  ta <-
    as.single(c(20, 20, 20, 1, 1, 1, 20, 20, 20, 20, 1, 1, 1, 1, 1, 1, 1, 1, 20, 20))
  tb <-
    as.single(c(20, 20, 20, 1, 1, 1, 20, 20, 20, 1,  1, 1, 1, 1, 1, 1, 1, 1, 20, 20))
  a <- Array(array(c(ta, tb), dim = c(20, 2)))
  out <- LongestStrikeBelowMean(a)
  b <- c(getData(out))
  expect_equal(b[1], 8, 1e-6)
  expect_equal(b[2], 9, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva Maximum tests")

test_that("Test Maximum", {
  ta <-
    as.single(c(20, 20, 20, 18, 25, 19, 20, 20, 20, 20, 40, 30, 1,  50, 1, 1,  5, 1, 20, 20))
  tb <-
    as.single(c(20, 20, 20, 2,  19, 1,  20, 20, 20, 1,  15, 1,  30, 1,  1, 18, 4, 1, 20, 20))
  a <- Array(array(c(ta, tb), dim = c(20, 2)))
  out <- Maximum(a)
  b <- c(getData(out))
  expect_equal(b[1], 50, 1e-6)
  expect_equal(b[2], 30, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva MeanAbsoluteChange tests")

test_that("Test MeanAbsoluteChange", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5))
  tb <- as.single(c(8, 10, 12, 14, 16, 18))
  a <- Array(array(c(ta, tb), dim = c(6, 2)))
  out <- MeanAbsoluteChange(a)
  b <- c(getData(out))
  expect_equal(b[1], 5 / 6, 1e-6)
  expect_equal(b[2], 10 / 6, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva FftCoefficient tests")

test_that("Test FftCoefficient", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5))
  tb <- as.single(c(6, 7, 8, 9, 10, 11))
  
  a <- Array(array(c(ta, tb), dim = c(6, 2)))
  out <- FftCoefficient(a, 0)
  real <- c(getData(out$real))
  imag <- c(getData(out$imag))
  abs <- c(getData(out$abs))
  angle <- c(getData(out$angle))
  
  expect_equal(real[1], 15, 1e-6)
  expect_equal(real[2], 51, 1e-6)
  
  expect_equal(imag[1], 0, 1e-6)
  expect_equal(imag[2], 0, 1e-6)
  
  expect_equal(abs[1], 15, 1e-6)
  expect_equal(abs[2], 51, 1e-6)
  
  expect_equal(angle[1], 0, 1e-6)
  expect_equal(angle[2], 0, 1e-6)
  deleteArray(a)
  deleteArray(out$real)
  deleteArray(out$imag)
  deleteArray(out$abs)
  deleteArray(out$angle)
})

context("Khiva AggregatedAutocorrelationMean tests")

test_that("Test AggregatedAutocorrelationMean", {
  # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <- as.single(c(1, 2, 3, 4, 5, 6))
    tb <- as.single(c(7, 8, 9, 10, 11, 12))
    a <- Array(array(c(ta, tb), dim = c(6, 2)))
    out <- AggregatedAutocorrelation(a, 0)
    b <- c(getData(out))
    expect_equal(b[1], -0.6571428571428571, 1e-6)
    expect_equal(b[2], -0.6571428571428571, 1e-6)
    deleteArray(a)
    deleteArray(out)
  }
})


context("Khiva AggregatedAutocorrelationMedian tests")

test_that("Test AggregatedAutocorrelationMedian", {
  # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <- as.single(c(1, 2, 3, 4, 5, 6))
    tb <- as.single(c(7, 8, 9, 10, 11, 12))
    a <- Array(array(c(ta, tb), dim = c(6, 2)))
    out <- AggregatedAutocorrelation(a, 1)
    b <- c(getData(out))
    expect_equal(b[1], -0.54285717010498047, 1e-6)
    expect_equal(b[2], -0.54285717010498047, 1e-6)
    deleteArray(a)
    deleteArray(out)
  }
})

context("Khiva AggregatedAutocorrelationMin tests")

test_that("Test AggregatedAutocorrelationMin", {
  # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <- as.single(c(1, 2, 3, 4, 5, 6))
    tb <- as.single(c(7, 8, 9, 10, 11, 12))
    a <- Array(array(c(ta, tb), dim = c(6, 2)))
    out <- AggregatedAutocorrelation(a, 2)
    b <- c(getData(out))
    expect_equal(b[1], -2.142857142857143, 1e-6)
    expect_equal(b[2], -2.142857142857143, 1e-6)
    deleteArray(a)
    deleteArray(out)
  }
})


context("Khiva AggregatedAutocorrelationMax tests")

test_that("Test AggregatedAutocorrelationMax", {
  # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <- as.single(c(1, 2, 3, 4, 5, 6))
    tb <- as.single(c(7, 8, 9, 10, 11, 12))
    a <- Array(array(c(ta, tb), dim = c(6, 2)))
    out <- AggregatedAutocorrelation(a, 3)
    b <- c(getData(out))
    expect_equal(b[1], 0.6, 1e-6)
    expect_equal(b[2], 0.6, 1e-6)
    deleteArray(a)
    deleteArray(out)
  }
})

context("Khiva AggregatedAutocorrelationStdev tests")

test_that("Test AggregatedAutocorrelationStdev", {
  # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <- as.single(c(1, 2, 3, 4, 5, 6))
    tb <- as.single(c(7, 8, 9, 10, 11, 12))
    a <- Array(array(c(ta, tb), dim = c(6, 2)))
    out <- AggregatedAutocorrelation(a, 4)
    b <- c(getData(out))
    expect_equal(b[1], 0.9744490855905009, 1e-6)
    expect_equal(b[2], 0.9744490855905009, 1e-6)
    deleteArray(a)
    deleteArray(out)
  }
})

context("Khiva AggregatedAutocorrelationVar tests")

test_that("Test AggregatedAutocorrelationVar", {
  # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <- as.single(c(1, 2, 3, 4, 5, 6))
    tb <- as.single(c(7, 8, 9, 10, 11, 12))
    a <- Array(array(c(ta, tb), dim = c(6, 2)))
    out <- AggregatedAutocorrelation(a, 5)
    b <- c(getData(out))
    expect_equal(b[1], 0.9495510204081633, 1e-6)
    expect_equal(b[2], 0.9495510204081633, 1e-6)
    deleteArray(a)
    deleteArray(out)
  }
})

context("Khiva AggregatedAutocorrelationMean tests")

test_that("Test AggregatedLinearTrendMean", {
  # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <- as.single(c(2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5))
    a <- Array(array(ta, dim = c(12, 1)))
    out <- AggregatedLinearTrend(a, 3, 0)
    slope <- c(getData(out$slope))
    intercept <- c(getData(out$intercept))
    rvalue <- c(getData(out$rvalue))
    pvalue <- c(getData(out$pvalue))
    stderrest <- c(getData(out$stderrest))
    expect_equal(slope[1], 1, 1e-6)

    expect_equal(intercept[1], 2, 1e-6)

    expect_equal(rvalue[1], 1, 1e-6)

    expect_equal(pvalue[1], 0, 1e-6)

    expect_equal(stderrest[1], 0, 1e-6)
    deleteArray(a)
    deleteArray(out$slope)
    deleteArray(out$intercept)
    deleteArray(out$rvalue)
    deleteArray(out$pvalue)
    deleteArray(out$stderrest)
  }
})

context("Khiva AggregatedLinearTrendMin tests")

test_that("Test AggregatedLinearTrendMin", {
  # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <- as.single(c(2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5))
    a <- Array(array(ta, dim = c(12, 1)))
    out <- AggregatedLinearTrend(a, 3, 2)
    slope <- c(getData(out$slope))
    intercept <- c(getData(out$intercept))
    rvalue <- c(getData(out$rvalue))
    pvalue <- c(getData(out$pvalue))
    stderrest <- c(getData(out$stderrest))
    expect_equal(slope[1], 1, 1e-3)

    expect_equal(intercept[1], 2, 1e-3)

    expect_equal(rvalue[1], 1, 1e-3)

    expect_equal(pvalue[1], 0, 1e-3)

    expect_equal(stderrest[1], 0, 1e-3)
    deleteArray(a)
    deleteArray(out$slope)
    deleteArray(out$intercept)
    deleteArray(out$rvalue)
    deleteArray(out$pvalue)
    deleteArray(out$stderrest)
  }
})

context("Khiva CwtCoefficients tests")

test_that("Test CwtCoefficients", {
   # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <- as.single(c(0.1, 0.2, 0.3))
    tb <- as.single(c(0.1, 0.2, 0.3))
    a <- Array(array(c(ta, tb), dim = c(3, 2)))
    z <- as.integer(c(1, 2, 3))
    tw <- Array(array(c(z), dim = c(3, 1)), "s32")
    out <- CwtCoefficients(a, tw, 2, 2)
    b <- c(getData(out))
    expect_equal(b[1], 0.26517161726951599, 1e-6)
    expect_equal(b[2], 0.26517161726951599, 1e-6)
    deleteArray(a)
    deleteArray(tw)
    deleteArray(out)
  }
})

context("Khiva MeanSecondDerivativeCentral tests")

test_that("Test MeanSecondDerivativeCentral", {
  ta <- as.single(c(1, 3, 7, 4, 8))
  tb <- as.single(c(2, 5, 1, 7, 4))
  a <- Array(array(c(ta, tb), dim = c(5, 2)))
  out <- MeanSecondDerivativeCentral(a)
  b <- c(getData(out))
  expect_equal(b[1], as.double(1.0 / 5.0), 1e-6)
  expect_equal(b[2], -3.0 / 5.0, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva Minimum tests")

test_that("Test Minimum", {
  ta <-
    as.single(c(
      20,
      20,
      20,
      18,
      25,
      19,
      20,
      20,
      20,
      20,
      40,
      30,
      1,
      50,
      13,
      15,
      5,
      16,
      20,
      20
    ))
  tb <-
    as.single(c(20, 20, 20, 2, 19, 4, 20, 20, 20, 4, 15, 6, 30, 7, 9, 18, 4, 10, 20, 20))
  a <- Array(array(c(ta, tb), dim = c(20, 2)))
  out <- Minimum(a)
  b <- c(getData(out))
  expect_equal(b[1], 1, 1e-6)
  expect_equal(b[2], 2, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva NumberCrossingM tests")

test_that("Test NumberCrossingM", {
  ta <-
    as.single(c(1, 2, 1, 1, -3, -4, 7, 8, 9, 10, -2, 1, -3, 5, 6, 7, -10))
  tb <-
    as.single(c(1, 2, 1, 1, -3, -4, 7, 8, 9, 10, -2, 1, -3, 5, 6, 7, -10))
  a <- Array(array(c(ta, tb), dim = c(17, 2)))
  out <- NumberCrossingM(a, 0)
  b <- c(getData(out))
  expect_equal(b[1], 7, 1e-6)
  expect_equal(b[2], 7, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva Mean tests")

test_that("Test Mean", {
  ta <-
    as.single(c(20, 20, 20, 18, 25, 19, 20, 20, 20, 20, 40, 30, 1, 50, 1, 1, 5, 1, 20, 20))
  tb <-
    as.single(c(20, 20, 20, 2, 19, 1, 20, 20, 20, 1, 15, 1, 30, 1, 1, 18, 4, 1, 20, 20))
  a <- Array(array(c(ta, tb), dim = c(20, 2)))
  out <- Mean(a)
  b <- c(getData(out))
  expect_equal(b[1], 18.55, 1e-6)
  expect_equal(b[2], 12.7, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva Median tests")

test_that("Test Median", {
  ta <-
    as.single(c(20, 20, 20, 18, 25, 19, 20, 20, 20, 20, 40, 30, 1, 50, 1, 1, 5, 1, 20, 20))
  tb <-
    as.single(c(20, 20, 20, 2, 19, 1, 20, 20, 20, 1, 15, 1, 30, 1, 1, 18, 4, 1, 20, 20))
  a <- Array(array(c(ta, tb), dim = c(20, 2)))
  out <- Median(a)
  b <- c(getData(out))
  expect_equal(b[1], 20, 1e-6)
  expect_equal(b[2], 18.5, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva MeanChange tests")

test_that("Test MeanChange", {
  ta <-
    as.single(c(0, 1, 2, 3, 4, 5))
  tb <-
    as.single(c(8, 10, 12, 14, 16, 18))
  a <- Array(array(c(ta, tb), dim = c(6, 2)))
  out <- MeanChange(a)
  b <- c(getData(out))
  expect_equal(b[1], 5 / 6, 1e-6)
  expect_equal(b[2], 10 / 6, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva MaxLangevinFixedPoint tests")

test_that("Test MaxLangevinFixedPoint", {
  ta <-
    as.single(c(0, 1, 2, 3, 4, 5))
  tb <-
    as.single(c(0, 1, 2, 3, 4, 5))
  a <- Array(array(c(ta, tb), dim = c(6, 2)))
  out <- MaxLangevinFixedPoint(a, 7, 2)
  b <- c(getData(out))
  expect_equal(b[1], 4.562970585, 1e-4)
  expect_equal(b[2], 4.562970585, 1e-4)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva FftAggregated tests")

test_that("Test FftAggregated", {
  # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <-
      as.single(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
    tb <-
      as.single(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
    a <- Array(array(c(ta, tb), dim = c(10, 2)))
    out <- FftAggregated(a)
    b <- c(getData(out))
    expect_equal(b[1], 1.135143, 1e-4)
    expect_equal(b[2], 2.368324, 1e-4)
    expect_equal(b[3], 1.248777, 1e-4)
    expect_equal(b[4], 3.642666, 1e-4)
    expect_equal(b[5], 1.135143, 1e-4)
    expect_equal(b[6], 2.368324, 1e-4)
    expect_equal(b[7], 1.248777, 1e-4)
    expect_equal(b[8], 3.642666, 1e-4)
    deleteArray(a)
    deleteArray(out)
  }
})

context("Khiva NumberPeaks tests")

test_that("Test NumberPeaks", {
  ta <-
    as.single(c(3, 0, 0, 4, 0, 0, 13))
  tb <-
    as.single(c(3, 0, 0, 4, 0, 0, 13))
  a <- Array(array(c(ta, tb), dim = c(7, 2)))
  out <- NumberPeaks(a, 2)
  b <- c(getData(out))
  expect_equal(b[1], 1, 1e-4)
  expect_equal(b[2], 1, 1e-4)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva PercentageOfReoccurringDatapointsToAllDatapoints tests")

test_that("Test PercentageOfReocurringDatapointsToAllDatapoints", {
  ta <-
    as.single(c(3, 0, 0, 4, 0, 0, 13))
  tb <-
    as.single(c(3, 0, 0, 4, 0, 0, 13))
  a <- Array(array(c(ta, tb), dim = c(7, 2)))
  out <-
    PercentageOfReoccurringDatapointsToAllDatapoints(a, FALSE)
  b <- c(getData(out))
  expect_equal(b[1], 0.25, 1e-4)
  expect_equal(b[2], 0.25, 1e-4)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva Quantile tests")

test_that("Test Quantile", {
  ta <-
    as.single(c(0, 0, 0, 0, 3, 4, 13))
  tb <-
    as.single(c(0, 0, 0, 0, 3, 4, 13))
  a <- Array(array(c(ta, tb), dim = c(7, 2)))
  quantile <- as.single(c(0.6))
  q.t <- Array(array(c(quantile), dim = c(1)))
  out <- Quantile(a, q.t)
  b <- getData(out)
  expect_equal(b[1], 1.79999999, 1e-4)
  expect_equal(b[2], 1.79999999, 1e-4)
  deleteArray(a)
  deleteArray(q.t)
  deleteArray(out)
})

context("Khiva RatioBeyondRSigma tests")

test_that("Test RatioBeyondRSigma", {
  # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
    if (travis.platform != 'osx') {
    ta <-
      as.single(c(3, 0, 0, 4, 0, 0, 13))
    tb <-
      as.single(c(3, 0, 0, 4, 0, 0, 13))
    a <- Array(array(c(ta, tb), dim = c(7, 2)))
    out <- RatioBeyondRSigma(a, 0.5)
    b <- c(getData(out))
    expect_equal(b[1], 0.7142857142857143, 1e-4)
    expect_equal(b[2], 0.7142857142857143, 1e-4)
    deleteArray(a)
    deleteArray(out)
  }
})

context("Khiva SampleEntropy tests")

test_that("Test SampleEntropy", {
   # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <-
      as.single(c(3, 0, 0, 4, 0, 0, 13))
    tb <-
      as.single(c(3, 0, 0, 4, 0, 0, 13))
    a <- Array(array(c(ta, tb), dim = c(7, 2)))
    out <- SampleEntropy(a)
    b <- c(getData(out))
    expect_equal(b[1], 1.252762968495368, 1e-4)
    expect_equal(b[2], 1.252762968495368, 1e-4)
    deleteArray(a)
    deleteArray(out)
  }
})

context("Khiva Skewness tests")

test_that("Test Skewness", {
   # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <-
      as.single(c(3, 0, 0, 4, 0, 0, 13))
    tb <-
      as.single(c(3, 0, 0, 4, 0, 0, 13))
    a <- Array(array(c(ta, tb), dim = c(7, 2)))
    out <- Skewness(a)
    b <- c(getData(out))
    expect_equal(b[1], 2.038404735373753, 1e-4)
    expect_equal(b[2], 2.038404735373753, 1e-4)
    deleteArray(a)
    deleteArray(out)
  }
})

context("Khiva StandardDeviation tests")

test_that("Test StandardDeviation", {
  ta <-
    as.single(c(20, 20, 20, 18, 25, 19, 20, 20, 20, 20, 40, 30, 1, 50, 1, 1, 5, 1, 20, 20))
  tb <-
    as.single(c(20, 20, 20, 2, 19, 1, 20, 20, 20, 1, 15, 1, 30, 1, 1, 18, 4, 1, 20, 20))
  a <- Array(array(c(ta, tb), dim = c(20, 2)))
  out <- StandardDeviation(a)
  b <- c(getData(out))
  expect_equal(b[1], 12.363150892875165, 1e-4)
  expect_equal(b[2], 9.51367436903324, 1e-4)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva SumOfReoccurringDatapoints tests")

test_that("Test SumOfReoccurringDatapoints", {
   # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <-
      as.single(c(3, 3, 0, 4, 0, 13, 13))
    tb <-
      as.single(c(3, 3, 0, 4, 0, 13, 13))
    a <- Array(array(c(ta, tb), dim = c(7, 2)))
    out <- SumOfReoccurringDatapoints(a)
    b <- c(getData(out))
    expect_equal(b[1], 32, 1e-4)
    expect_equal(b[2], 32, 1e-4)
    deleteArray(a)
    deleteArray(out)
  }
})

context("Khiva SymmetryLooking tests")

test_that("Test SymmetryLooking", {
  ta <-
    as.single(c(20, 20, 20, 18, 25, 19, 20, 20, 20, 20, 40, 30, 1, 50, 1, 1, 5, 1, 20, 20))
  tb <-
    as.single(c(20, 20, 20, 2, 19, 1, 20, 20, 20, 1, 15, 1, 30, 1, 1, 18, 4, 1, 20, 20))
  a <- Array(array(c(ta, tb), dim = c(20, 2)))
  out <- SymmetryLooking(a, 0.1)
  b <- c(getData(out))
  expect_equal(b[1], TRUE)
  expect_equal(b[2], FALSE)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva ValueCount tests")

test_that("Test ValueCount", {
  ta <-
    as.single(c(20, 20, 20, 18, 25, 19, 20, 20, 20, 20, 40, 30, 1, 50, 1, 1, 5, 1, 20, 20))
  tb <-
    as.single(c(20, 20, 20, 2, 19, 1, 20, 20, 20, 1, 15, 1, 30, 1, 1, 18, 4, 1, 20, 20))
  a <- Array(array(c(ta, tb), dim = c(20, 2)))
  out <- ValueCount(a, 20)
  b <- c(getData(out))
  expect_equal(b[1], 9, 1e-6)
  expect_equal(b[2], 8, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva FriedrichCoefficients tests")

test_that("Test FriedrichCoefficients", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5))
  tb <- as.single(c(0, 1, 2, 3, 4, 5))
  a <- Array(array(c(ta, tb), dim = c(6, 2)))
  out <- FriedrichCoefficients(a, 4, 2)
  b <- c(getData(out))
  expected <- c(
    -0.0009912563255056738,-0.0027067768387496471,
    -0.00015192681166809052,
    0.10512571036815643,
    0.89872437715530396,-0.0009912563255056738,
    -0.0027067768387496471,-0.00015192681166809052,
    0.10512571036815643,
    0.89872437715530396
  )
  expect_equal(b, expected, 1e-6)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva NumberCwtPeaks tests")

test_that("Test NumberCwtPeaks", {
   # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <-
      as.single(c(1, 1, 1, 1, 1, 1, 1, 5, 1, 1, 1, 1, 1, 1, 5, 1, 1, 1, 1, 1, 1))
    tb <-
      as.single(c(1, 1, 1, 1, 1, 1, 1, 5, 1, 1, 1, 1, 1, 1, 5, 1, 1, 1, 1, 1, 1))
    a <- Array(array(c(ta, tb), dim = c(22, 2)))
    out <- NumberCwtPeaks(a, 2)
    b <- c(getData(out))
    expect_equal(b[1], 2)
    expect_equal(b[2], 2)
    deleteArray(a)
    deleteArray(out)
  }
})

context("Khiva PartialAutocorrelation tests")

test_that("Test PartialAutocorrelation", {
   # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    numel <- 3000
    step <- 1 / (numel - 1)
    ta <- as.single(seq(length = 3000))
    for (i in 0:2999) {
      ta[i + 1] = step * (i)
    }
    a <- Array(array(c(ta, ta), dim = c(3000, 2)))
    tlag <- as.integer(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
    lags <- Array(array(c(tlag), dim = c(10, 1)), "s32")
    out <- PartialAutocorrelation(a, lags)
    b <- c(getData(out))
    expected <-
      c(
        1.0,
        0.9993331432342529,
        -0.0006701064994559,
        -0.0006701068487018,
        -0.0008041285327636,
        -0.0005360860959627,
        -0.0007371186511591,
        -0.0004690756904893,
        -0.0008041299879551,
        -0.0007371196406893,
        1.0,
        0.9993331432342529,
        -0.0006701064994559,
        -0.0006701068487018,
        -0.0008041285327636,
        -0.0005360860959627,
        -0.0007371186511591,
        -0.0004690756904893,
        -0.0008041299879551,
        -0.0007371196406893
      )
    expect_equal(b, expected, 1e-3)
    deleteArray(a)
    deleteArray(lags)
    deleteArray(out)
  }
})

context("Khiva PercentageOfReoccurringValuesToAllValues tests")

test_that("Test PercentageOfReoccurringValuesToAllValues", {
   # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <- as.single(c(1, 1, 2, 3, 4, 4, 5, 6))
    tb <- as.single(c(1, 2, 2, 3, 4, 5, 6, 7))
    a <- Array(array(c(ta, tb), dim = c(8, 2)))
    out <- PercentageOfReoccurringValuesToAllValues(a, FALSE)
    b <- c(getData(out))
    expect_equal(b[1], 0.5)
    expect_equal(b[2], 0.25)
    deleteArray(a)
    deleteArray(out)
  }
})

context("Khiva RangeCount tests")

test_that("Test RangeCount", {
  ta <- as.single(c(3, 0, 0, 4, 0, 0, 13))
  tb <- as.single(c(3, 0, 5, 4, 0, 0, 13))
  a <- Array(array(c(ta, tb), dim = c(7, 2)))
  out <- RangeCount(a, 2, 12)
  b <- c(getData(out))
  expect_equal(b[1], 2)
  expect_equal(b[2], 3)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva RatioValueNumberToTimeSeriesLength tests")

test_that("Test RatioValueNumberToTimeSeriesLength", {
   # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <- as.single(c(3, 0, 0, 4, 0, 0, 13))
    tb <- as.single(c(3, 5, 0, 4, 6, 0, 13))
    a <- Array(array(c(ta, tb), dim = c(7, 2)))
    out <- RatioValueNumberToTimeSeriesLength(a)
    b <- c(getData(out))
    expect_equal(b[1], 4 / 7, 1e-4)
    expect_equal(b[2], 6 / 7, 1e-4)
    deleteArray(a)
    deleteArray(out)
  }
})

context("Khiva SpktWelchDensity tests")

test_that("Test SpktWelchDensity", {
   # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <- as.single(c(0, 1, 1, 3, 4, 5, 6, 7, 8, 9))
    tb <- as.single(c(0, 1, 1, 3, 4, 5, 6, 7, 8, 9))
    a <- Array(array(c(ta, tb), dim = c(10, 2)))
    out <- SpktWelchDensity(a, 0)
    b <- getData(out)
    expect_equal(b[1], 1.6666667461395264, 1e-5)
    expect_equal(b[2], 1.6666667461395264, 1e-5)
    deleteArray(a)
    deleteArray(out)
  }
})

context("Khiva SumOfReoccurringValues tests")

test_that("Test SumOfReocurringValues", {
   # Filtering this test in travis for OSX. Problem inside ArrayFire
  travis.platform <- Sys.getenv("TRAVIS_OS_NAME")
  if (travis.platform != 'osx') {
    ta <- as.single(c(4, 4, 6, 6, 7))
    tb <- as.single(c(4, 7, 7, 8, 8))
    a <- Array(array(c(ta, tb), dim = c(5, 2)))
    out <- SumOfReoccurringValues(a)
    b <- c(getData(out))
    expect_equal(b[1], 10, 1e-5)
    expect_equal(b[2], 15, 1e-5)
    deleteArray(a)
    deleteArray(out)
  }
})

context("Khiva SumValues tests")

test_that("Test SumValues", {
  ta <- as.single(c(1, 2, 3, 4.1))
  tb <- as.single(c(-1.2, -2, -3, -4))
  a <- Array(array(c(ta, tb), dim = c(4, 2)))
  out <- SumValues(a)
  b <- c(getData(out))
  expect_equal(b[1], 10.1, 1e-5)
  expect_equal(b[2], -10.2, 1e-5)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva TimeReversalAsymmetryStatistic tests")

test_that("Test TimeReversalAsymmetryStatistic", {
  ta <-
    as.single(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20))
  tb <-
    as.single(c(20, 20, 20, 2, 19, 1, 20, 20, 20, 1, 15, 1, 30, 1, 1, 18, 4, 1, 20, 20))
  a <- Array(array(c(ta, tb), dim = c(20, 2)))
  out <- TimeReversalAsymmetryStatistic(a, 2)
  b <- c(getData(out))
  expect_equal(b[1], 1052)
  expect_equal(b[2], -150.625)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva Variance tests")

test_that("Test Variance", {
  ta <- as.single(c(1, 1, -1, -1))
  tb <- as.single(c(1, 2, -2, -1))
  a <- Array(array(c(ta, tb), dim = c(4, 2)))
  out <- Variance(a)
  b <- c(getData(out))
  expect_equal(b[1], 1)
  expect_equal(b[2], 2.5)
  deleteArray(a)
  deleteArray(out)
})

context("Khiva VarianceLargerThanStandardDeviation tests")

test_that("Test VarianceLargerThanStandardDeviation", {
  ta <-
    as.single(c(20, 20, 20, 18, 25, 19, 20, 20, 20, 20, 40, 30, 1, 50, 1, 1, 5, 1, 20, 20))
  tb <-
    as.single(c(20, 20, 20, 2, 19, 1, 20, 20, 20, 1, 15, 1, 30, 1, 1, 18, 4, 1, 20, 20))
  a <- Array(array(c(ta, tb), dim = c(20, 2)))
  out <- VarianceLargerThanStandardDeviation(a)
  b <- c(getData(out))
  expect_equal(b[1], TRUE)
  expect_equal(b[2], TRUE)
  deleteArray(a)
  deleteArray(out)
})