#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

test_that("Test C3", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5))
  tb <- as.single(c(6, 7, 8, 9, 10, 11))
  a <- Array(data.frame(ta, tb))
  out <- C3(a, 2)
  b <- getData(out)
  expect_equal(b[1], 7.5, 1e-6)
  expect_equal(b[2], 586.5, 1e-6)
})

test_that("Test CidCe", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5))
  tb <- as.single(c(6, 7, 8, 9, 10, 11))
  a <- Array(data.frame(ta, tb))
  out <- CidCe(a, TRUE)
  b <- getData(out)
  expect_equal(b[1], 1.30930734141595, 1e-6)
  expect_equal(b[2], 1.30930734141595, 1e-6)
  
  out <- CidCe(a, FALSE)
  b <- getData(out)
  expect_equal(b[1], 2.23606797749979, 1e-6)
  expect_equal(b[2], 2.23606797749979, 1e-6)
})

test_that("Test AbsEnergy", {
  ta <- as.single(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  a <- Array(data.frame(ta))
  out <- AbsEnergy(a)
  b <- getData(out)
  expect_equal(b[1], 385, 1e-6)
})

test_that("Test AbsoluteSumOfChanges", {
  ta <- as.single(c(0, 1, 2, 3))
  tb <- as.single(c(4, 6, 8, 10))
  tc <- as.single(c(11, 14, 17, 20))
  a <- Array(data.frame(ta, tb, tc))
  out <- AbsoluteSumOfChanges(a)
  b <- getData(out)
  expect_equal(b[1], 3, 1e-6)
  expect_equal(b[2], 6, 1e-6)
  expect_equal(b[3], 9, 1e-6)
})

test_that("Test CrossCorrelation", {
  ta <- as.single(c(1, 2, 3, 4))
  tb <- as.single(c(4, 6, 8, 10, 12))
  a <- Array(data.frame(ta))
  b <- Array(data.frame(tb))
  out <- CrossCorrelation(a, b, FALSE)
  c <- getData(out)
  expect_equal(c[1], 0.790569415, 1e-6)
  expect_equal(c[2], 0.790569415, 1e-6)
  expect_equal(c[3], 0.079056941, 1e-6)
  expect_equal(c[4], -0.395284707, 1e-6)
  expect_equal(c[5], -0.474341649, 1e-6)
})

test_that("Test AutoCovariance", {
  ta <- as.single(c(0, 1, 2, 3))
  tb <- as.single(c(10, 11, 12, 13))
  a <- Array(data.frame(ta, tb))
  out <- AutoCovariance(a, FALSE)
  b <- getData(out)
  expect_equal(b[1], 1.25, 1e-6)
  expect_equal(b[2], 0.3125, 1e-6)
  expect_equal(b[3], -0.375, 1e-6)
  expect_equal(b[4], -0.5625, 1e-6)
  expect_equal(b[5], 1.25, 1e-6)
  expect_equal(b[6], 0.3125, 1e-6)
  expect_equal(b[7], -0.375, 1e-6)
  expect_equal(b[8], -0.5625, 1e-6)
})

test_that("Test CrossCovariance", {
  ta <- as.single(c(0, 1, 2, 3))
  tb <- as.single(c(10, 11, 12, 13))
  tc <- as.single(c(4, 6, 8, 10, 12))
  td <- as.single(c(14, 16, 18, 20, 22))
  a <- Array(data.frame(ta, tb))
  b <- Array(data.frame(tc, td))
  
  out <- CrossCovariance(a, b, FALSE)
  
  c <- getData(out)
  for (i in 0:3) {
    expect_equal(c[(i * 5) + 1], 2.5, 1e-6)
    expect_equal(c[(i * 5) + 2], 2.5, 1e-6)
    expect_equal(c[(i * 5) + 3], 0.25, 1e-6)
    expect_equal(c[(i * 5) + 4], -1.25, 1e-6)
    expect_equal(c[(i * 5) + 5], -1.5, 1e-6)
  }
})

test_that("Test ApproximateEntropy", {
  ta <- as.single(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  tb <- as.single(c(11, 12, 13, 14, 15, 16, 17, 18, 19, 20))
  a <- Array(data.frame(ta, tb))
  out <- ApproximateEntropy(a, 4, 0.5)
  b <- getData(out)
  expect_equal(b[1], 0.13484275341033936, 1e-6)
  expect_equal(b[2], 0.13484275341033936, 1e-6)
})

test_that("Test AutoCorrelation", {
  ta <- as.single(c(0, 1, 2, 3))
  tb <- as.single(c(10, 11, 12, 13))
  
  a <- Array(data.frame(ta, tb))
  out <- AutoCorrelation(a, 4, FALSE)
  c <- getData(out)
  expect_equal(c[1], 1, 1e-6)
  expect_equal(c[2], 0.25, 1e-6)
  expect_equal(c[3],-0.3, 1e-6)
  expect_equal(c[4],-0.45, 1e-6)
  expect_equal(c[5], 1, 1e-6)
  expect_equal(c[6], 0.25, 1e-6)
  expect_equal(c[7],-0.3, 1e-6)
  expect_equal(c[8],-0.45, 1e-6)
})

test_that("Test BinnedEntropy", {
  ta <- as.single(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
                    14, 15, 16, 17, 18, 19, 20))
  tb <-
    as.single(c(1, 1, 3, 10, 5, 6, 1, 8, 9, 10, 11, 1, 13, 14, 10, 16, 17, 10, 19,
                20))
  a <- Array(data.frame(ta, tb))
  
  out <- BinnedEntropy(a, 5)
  c <- getData(out)
  expect_equal(c[1], 1.6094379124341005, 1e-6)
  expect_equal(c[2], 1.5614694247763998, 1e-6)
})

test_that("Test CountAboveMean", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5))
  tb <- as.single(c(6, 7, 8, 9, 10, 11))
  a <- Array(data.frame(ta, tb))
  
  out <- CountAboveMean(a)
  
  c <- getData(out)
  expect_equal(c[1], 3, 1e-6)
  expect_equal(c[2], 3, 1e-6)
})

test_that("Test CountBelowMean", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5))
  tb <- as.single(c(6, 7, 8, 9, 10, 11))
  a <- Array(data.frame(ta, tb))
  out <- CountBelowMean(a)
  b <- getData(out)
  expect_equal(b[1], 3, 1e-6)
  expect_equal(b[2], 3, 1e-6)
})

test_that("Test EnergyRatioBychunks", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5))
  tb <- as.single(c(6, 7, 8, 9, 10, 11))
  
  a <- Array(data.frame(ta, tb))
  out <- EnergyRatioByChunks(a, 2, 0)
  b <- getData(out)
  expect_equal(b[1], 0.090909091, 1e-6)
  expect_equal(b[2], 0.330376940, 1e-6)
  
  out <- EnergyRatioByChunks(a, 2, 1)
  b <- getData(out)
  
  expect_equal(b[1], 0.909090909, 1e-6)
  expect_equal(b[2], 0.669623060, 1e-6)
})

test_that("Test CountBelowMean", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5))
  tb <- as.single(c(6, 7, 8, 9, 10, 11))
  a <- Array(data.frame(ta, tb))
  out <- CountBelowMean(a)
  b <- getData(out)
  expect_equal(b[1], 3, 1e-6)
  expect_equal(b[2], 3, 1e-6)
})

test_that("Test FirstLocationOfMaximum", {
  ta <- as.single(c(5, 4, 3, 5, 0, 1, 5, 3, 2, 1))
  tb <- as.single(c(2, 4, 3, 5, 2, 5, 4, 3, 5, 2))
  a <- Array(data.frame(ta, tb))
  out <- FirstLocationOfMaximum(a)
  c <- getData(out)
  expect_equal(c[1], 0.0, 1e-6)
  expect_equal(c[2], 0.3, 1e-6)
})

test_that("Test FirstLocationOfMinimum", {
  ta <- as.single(c(5, 4, 3, 0, 0, 1))
  tb <- as.single(c(5, 4, 3, 0, 2, 1))
  a <- Array(data.frame(ta, tb))
  out <- FirstLocationOfMinimum(a)
  c <- getData(out)
  expect_equal(c[1], 0.5, 1e-6)
  expect_equal(c[2], 0.5, 1e-6)
})

test_that("Test HasDuplicates", {
  ta <- as.single(c(5, 4, 3, 0, 0, 1))
  tb <- as.single(c(5, 4, 3, 0, 2, 1))
  a <- Array(data.frame(ta, tb))
  
  out <- HasDuplicates(a)
  c <- getData(out)
  
  expect_equal(c[1], TRUE)
  expect_equal(c[2], FALSE)
})

test_that("Test HasDuplicateMax", {
  ta <- as.single(c(5, 4, 3, 0, 5, 1))
  tb <- as.single(c(5, 4, 3, 0, 2, 1))
  a <- Array(data.frame(ta, tb))
  
  out <- HasDuplicateMax(a)
  c <- getData(out)
  
  expect_equal(c[1], TRUE)
  expect_equal(c[2], FALSE)
})

test_that("Test IndexMassQuantile", {
  ta <- as.single(c(5, 4, 3, 0, 5, 1))
  tb <- as.single(c(5, 4, 3, 0, 2, 1))
  a <- Array(data.frame(ta, tb))
  
  out <- IndexMassQuantile(a, 0.5)
  c <- getData(out)
  
  expect_equal(c[1], 0.333333333, 1e-6)
  expect_equal(c[2], 0.333333333, 1e-6)
})

test_that("Test Kurtosis", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5))
  tb <- as.single(c(2, 2, 2, 20, 30, 25))
  a <- Array(data.frame(ta, tb))
  
  out <- Kurtosis(a)
  c <- getData(out)
  
  expect_equal(c[1], -1.2, 1e-6)
  expect_equal(c[2], -2.66226722, 1e-6)
})

test_that("Test LargeStandardDeviation", {
  ta <- as.single(c(-1, -1, -1, 1, 1, 1))
  tb <- as.single(c(4, 6, 8, 4, 5, 4))
  a <- Array(data.frame(ta, tb))
  
  out <- LargeStandardDeviation(a, 0.4)
  c <- getData(out)
  
  expect_equal(c[1], TRUE)
  expect_equal(c[2], FALSE)
})

test_that("Test LastLocationOfMaximum", {
  ta <- as.single(c(0, 4, 3, 5, 5, 1))
  tb <- as.single(c(0, 4, 3, 2, 5, 1))
  a <- Array(data.frame(ta, tb))
  out <- LastLocationOfMaximum(a)
  c <- getData(out)
  
  expect_equal(c[1], 0.8333333333333334, 1e-6)
  expect_equal(c[2], 0.8333333333333334, 1e-6)
})

test_that("Test LastLocationOfMinimum", {
  ta <- as.single(c(0, 4, 3, 5, 5, 1, 0, 4))
  tb <- as.single(c(3, 2, 5, 1, 4, 5, 1, 2))
  a <- Array(data.frame(ta, tb))
  
  out <- LastLocationOfMinimum(a)
  c <- getData(out)
  
  expect_equal(c[1], 0.875, 1e-6)
  expect_equal(c[2], 0.875, 1e-6)
})

test_that("Test Length", {
  ta <- as.single(c(0, 4, 3, 5, 5, 1))
  tb <- as.single(c(0, 4, 3, 2, 5, 1))
  a <- Array(data.frame(ta, tb))
  
  out <- Length(a)
  c <- getData(out)
  
  expect_equal(c[1], 6, 1e-6)
  expect_equal(c[2], 6, 1e-6)
})

test_that("Test LinearTrend", {
  ta <- as.single(c(0, 4, 3, 5, 5, 1))
  tb <- as.single(c(2, 4, 1, 2, 5, 3))
  a <- Array(data.frame(ta, tb))
  out <- LinearTrend(a)
  pvalue <- getData(out$pvalue)
  rvalue <- getData(out$rvalue)
  intercept <- getData(out$intercept)
  slope <- getData(out$slope)
  stdrr <- getData(out$stdrr)
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
})

test_that("Test HasDuplicateMin", {
  ta <- as.single(c(5, 4, 3, 0, 0, 1))
  tb <- as.single(c(5, 4, 3, 0, 2, 1))
  a <- Array(data.frame(ta, tb))
  out <- HasDuplicateMin(a)
  b <- getData(out)
  expect_equal(b[1], TRUE)
  expect_equal(b[2], FALSE)
})

test_that("Test LongestStrikeAboveMean", {
  ta <-
    as.single(c(20, 20, 20, 1, 1, 1, 20, 20, 20, 20, 1, 1, 1, 1, 1, 1, 1, 1, 20, 20))
  tb <-
    as.single(c(20, 20, 20, 1, 1, 1, 20, 20, 20, 1,  1, 1, 1, 1, 1, 1, 1, 1, 20, 20))
  a <- Array(data.frame(ta, tb))
  out <- LongestStrikeAboveMean(a)
  b <- getData(out)
  expect_equal(b[1], 4, 1e-6)
  expect_equal(b[2], 3, 1e-6)
})

test_that("Test LongestStrikeBelowMean", {
  ta <-
    as.single(c(20, 20, 20, 1, 1, 1, 20, 20, 20, 20, 1, 1, 1, 1, 1, 1, 1, 1, 20, 20))
  tb <-
    as.single(c(20, 20, 20, 1, 1, 1, 20, 20, 20, 1,  1, 1, 1, 1, 1, 1, 1, 1, 20, 20))
  a <- Array(data.frame(ta, tb))
  out <- LongestStrikeBelowMean(a)
  b <- getData(out)
  expect_equal(b[1], 8, 1e-6)
  expect_equal(b[2], 9, 1e-6)
})

test_that("Test Maximum", {
  ta <-
    as.single(c(20, 20, 20, 18, 25, 19, 20, 20, 20, 20, 40, 30, 1,  50, 1, 1,  5, 1, 20, 20))
  tb <-
    as.single(c(20, 20, 20, 2,  19, 1,  20, 20, 20, 1,  15, 1,  30, 1,  1, 18, 4, 1, 20, 20))
  a <- Array(data.frame(ta, tb))
  out <- Maximum(a)
  b <- getData(out)
  expect_equal(b[1], 50, 1e-6)
  expect_equal(b[2], 30, 1e-6)
})

test_that("Test MeanAbsoluteChange", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5))
  tb <- as.single(c(8, 10, 12, 14, 16, 18))
  a <- Array(data.frame(ta, tb))
  out <- MeanAbsoluteChange(a)
  b <- getData(out)
  expect_equal(b[1], 5 / 6, 1e-6)
  expect_equal(b[2], 10 / 6, 1e-6)
})

test_that("Test FftCoefficient", {
  ta <- as.single(c(0, 1, 2, 3, 4, 5))
  tb <- as.single(c(6, 7, 8, 9, 10, 11))
  
  a <- Array(data.frame(ta, tb))
  out <- FftCoefficient(a, 0)
  real <- getData(out$real)
  imag <- getData(out$imag)
  abs <- getData(out$abs)
  angle <- getData(out$angle)
  
  expect_equal(real[1], 15, 1e-6)
  expect_equal(real[2], 51, 1e-6)
  
  expect_equal(imag[1], 0, 1e-6)
  expect_equal(imag[2], 0, 1e-6)
  
  expect_equal(abs[1], 15, 1e-6)
  expect_equal(abs[2], 51, 1e-6)
  
  expect_equal(angle[1], 0, 1e-6)
  expect_equal(angle[2], 0, 1e-6)
})

test_that("Test AggregatedAutocorrelationMean", {
  ta <- as.single(c(1, 2, 3, 4, 5, 6))
  tb <- as.single(c(7, 8, 9, 10, 11, 12))
  a <- Array(data.frame(ta, tb))
  out <- AggregatedAutocorrelation(a, 0)
  b <- getData(out)
  expect_equal(b[1], -0.6571428571428571, 1e-6)
  expect_equal(b[2], -0.6571428571428571, 1e-6)
})

test_that("Test AggregatedAutocorrelationMedian", {
  ta <- as.single(c(1, 2, 3, 4, 5, 6))
  tb <- as.single(c(7, 8, 9, 10, 11, 12))
  a <- Array(data.frame(ta, tb))
  out <- AggregatedAutocorrelation(a, 1)
  b <- getData(out)
  expect_equal(b[1], -0.54285717010498047, 1e-6)
  expect_equal(b[2], -0.54285717010498047, 1e-6)
})

test_that("Test AggregatedAutocorrelationMin", {
  ta <- as.single(c(1, 2, 3, 4, 5, 6))
  tb <- as.single(c(7, 8, 9, 10, 11, 12))
  a <- Array(data.frame(ta, tb))
  out <- AggregatedAutocorrelation(a, 2)
  b <- getData(out)
  expect_equal(b[1], -2.142857142857143, 1e-6)
  expect_equal(b[2], -2.142857142857143, 1e-6)
})

test_that("Test AggregatedAutocorrelationMax", {
  ta <- as.single(c(1, 2, 3, 4, 5, 6))
  tb <- as.single(c(7, 8, 9, 10, 11, 12))
  a <- Array(data.frame(ta, tb))
  out <- AggregatedAutocorrelation(a, 3)
  b <- getData(out)
  expect_equal(b[1], 0.6, 1e-6)
  expect_equal(b[2], 0.6, 1e-6)
})

test_that("Test AggregatedAutocorrelationStdev", {
  ta <- as.single(c(1, 2, 3, 4, 5, 6))
  tb <- as.single(c(7, 8, 9, 10, 11, 12))
  a <- Array(data.frame(ta, tb))
  out <- AggregatedAutocorrelation(a, 4)
  b <- getData(out)
  expect_equal(b[1], 0.9744490855905009, 1e-6)
  expect_equal(b[2], 0.9744490855905009, 1e-6)
})

test_that("Test AggregatedAutocorrelationVar", {
  ta <- as.single(c(1, 2, 3, 4, 5, 6))
  tb <- as.single(c(7, 8, 9, 10, 11, 12))
  a <- Array(data.frame(ta, tb))
  out <- AggregatedAutocorrelation(a, 5)
  b <- getData(out)
  expect_equal(b[1], 0.9495510204081633, 1e-6)
  expect_equal(b[2], 0.9495510204081633, 1e-6)
})

test_that("Test AggregatedLinearTrendMean", {
  ta <- as.single(c(2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5))
  a <- Array(data.frame(ta))
  out <- AggregatedLinearTrend(a, 3, 0)
  slope <- getData(out$slope)
  intercept <- getData(out$intercept)
  rvalue <- getData(out$rvalue)
  pvalue <- getData(out$pvalue)
  stderrest <- getData(out$stderrest)
  expect_equal(slope[1], 1, 1e-6)
  
  expect_equal(intercept[1], 2, 1e-6)
  
  expect_equal(rvalue[1], 1, 1e-6)
  
  expect_equal(pvalue[1], 0, 1e-6)
  
  expect_equal(stderrest[1], 0, 1e-6)
})

test_that("Test AggregatedLinearTrendMin", {
  ta <- as.single(c(2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5))
  a <- Array(data.frame(ta))
  out <- AggregatedLinearTrend(a, 3, 2)
  slope <- getData(out$slope)
  intercept <- getData(out$intercept)
  rvalue <- getData(out$rvalue)
  pvalue <- getData(out$pvalue)
  stderrest <- getData(out$stderrest)
  expect_equal(slope[1], 1, 1e-3)
  
  expect_equal(intercept[1], 2, 1e-3)
  
  expect_equal(rvalue[1], 1, 1e-3)
  
  expect_equal(pvalue[1], 0, 1e-3)
  
  expect_equal(stderrest[1], 0, 1e-3)
})

test_that("Test CwtCoefficients", {
  ta <- as.single(c(0.1, 0.2, 0.3))
  tb <- as.single(c(0.1, 0.2, 0.3))
  a <- Array(data.frame(ta, tb))
  z <- data.frame(as.integer(c(1, 2, 3)))
  tw <- Array(z)
  out <- CwtCoefficients(a, tw, 2, 2)
  b <- getData(out)
  expect_equal(b[1], 0.26517161726951599, 1e-6)
  expect_equal(b[2], 0.26517161726951599, 1e-6)
})

test_that("Test MeanSecondDerivativeCentral", {
  ta <- as.single(c(1, 3, 7, 4, 8))
  tb <- as.single(c(2, 5, 1, 7, 4))
  a <- Array(data.frame(ta, tb))
  out <- MeanSecondDerivativeCentral(a)
  b <- getData(out)
  expect_equal(b[1], as.double(1.0 / 5.0), 1e-6)
  expect_equal(b[2], -3.0 / 5.0, 1e-6)
})

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
  a <- Array(data.frame(ta, tb))
  out <- Minimum(a)
  b <- getData(out)
  expect_equal(b[1], 1, 1e-6)
  expect_equal(b[2], 2, 1e-6)
})

test_that("Test NumberCrossingM", {
  ta <-
    as.single(c(1, 2, 1, 1, -3, -4, 7, 8, 9, 10, -2, 1, -3, 5, 6, 7, -10))
  tb <-
    as.single(c(1, 2, 1, 1, -3, -4, 7, 8, 9, 10, -2, 1, -3, 5, 6, 7, -10))
  a <- Array(data.frame(ta, tb))
  out <- NumberCrossingM(a, 0)
  b <- getData(out)
  expect_equal(b[1], 7, 1e-6)
  expect_equal(b[2], 7, 1e-6)
})

test_that("Test Mean", {
  ta <-
    as.single(c(20, 20, 20, 18, 25, 19, 20, 20, 20, 20, 40, 30, 1, 50, 1, 1, 5, 1, 20, 20))
  tb <-
    as.single(c(20, 20, 20, 2, 19, 1, 20, 20, 20, 1, 15, 1, 30, 1, 1, 18, 4, 1, 20, 20))
  a <- Array(data.frame(ta, tb))
  out <- Mean(a)
  b <- getData(out)
  expect_equal(b[1], 18.55, 1e-6)
  expect_equal(b[2], 12.7, 1e-6)
})

test_that("Test Median", {
  ta <-
    as.single(c(20, 20, 20, 18, 25, 19, 20, 20, 20, 20, 40, 30, 1, 50, 1, 1, 5, 1, 20, 20))
  tb <-
    as.single(c(20, 20, 20, 2, 19, 1, 20, 20, 20, 1, 15, 1, 30, 1, 1, 18, 4, 1, 20, 20))
  a <- Array(data.frame(ta, tb))
  out <- Median(a)
  b <- getData(out)
  expect_equal(b[1], 20, 1e-6)
  expect_equal(b[2], 18.5, 1e-6)
})

test_that("Test MeanChange", {
  ta <-
    as.single(c(0, 1, 2, 3, 4, 5))
  tb <-
    as.single(c(8, 10, 12, 14, 16, 18))
  a <- Array(data.frame(ta, tb))
  out <- MeanChange(a)
  b <- getData(out)
  expect_equal(b[1], 5 / 6, 1e-6)
  expect_equal(b[2], 10 / 6, 1e-6)
})

test_that("Test MaxLangevinFixedPoint", {
  ta <-
    as.single(c(0, 1, 2, 3, 4, 5))
  tb <-
    as.single(c(0, 1, 2, 3, 4, 5))
  a <- Array(data.frame(ta, tb))
  out <- MaxLangevinFixedPoint(a, 7, 2)
  b <- getData(out)
  expect_equal(b[1], 4.562970585, 1e-4)
  expect_equal(b[2], 4.562970585, 1e-4)
})

test_that("Test FftAggregated", {
  ta <-
    as.single(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
  tb <-
    as.single(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
  a <- Array(data.frame(ta, tb))
  out <- FftAggregated(a)
  b <- getData(out)
  expect_equal(b[1], 1.135143, 1e-4)
  expect_equal(b[2], 2.368324, 1e-4)
  expect_equal(b[3], 1.248777, 1e-4)
  expect_equal(b[4], 3.642666, 1e-4)
  expect_equal(b[5], 1.135143, 1e-4)
  expect_equal(b[6], 2.368324, 1e-4)
  expect_equal(b[7], 1.248777, 1e-4)
  expect_equal(b[8], 3.642666, 1e-4)
})

test_that("Test NumberPeaks", {
  ta <-
    as.single(c(3, 0, 0, 4, 0, 0, 13))
  tb <-
    as.single(c(3, 0, 0, 4, 0, 0, 13))
  a <- Array(data.frame(ta, tb))
  out <- NumberPeaks(a, 2)
  b <- getData(out)
  expect_equal(b[1], 1, 1e-4)
  expect_equal(b[2], 1, 1e-4)
})

test_that("Test PercentageOfReocurringDatapointsToAllDatapoints", {
  ta <-
    as.single(c(3, 0, 0, 4, 0, 0, 13))
  tb <-
    as.single(c(3, 0, 0, 4, 0, 0, 13))
  a <- Array(data.frame(ta, tb))
  out <-
    PercentageOfReoccurringDatapointsToAllDatapoints(a, FALSE)
  b <- getData(out)
  expect_equal(b[1], 0.25, 1e-4)
  expect_equal(b[2], 0.25, 1e-4)
})

test_that("Test Quantile", {
  ta <-
    as.single(c(0, 0, 0, 0, 3, 4, 13))
  tb <-
    as.single(c(0, 0, 0, 0, 3, 4, 13))
  a <- Array(data.frame(ta, tb))
  quantile <- as.single(c(0.6))
  q.t <- Array(data.frame(quantile))
  out <- Quantile(a, q.t)
  b <- getData(out)
  expect_equal(b[1], 1.79999999, 1e-4)
  expect_equal(b[2], 1.79999999, 1e-4)
})

test_that("Test RatioBeyondRSigma", {
  ta <-
    as.single(c(3, 0, 0, 4, 0, 0, 13))
  tb <-
    as.single(c(3, 0, 0, 4, 0, 0, 13))
  a <- Array(data.frame(ta, tb))
  out <- RatioBeyondRSigma(a, 0.5)
  b <- getData(out)
  expect_equal(b[1], 0.7142857142857143, 1e-4)
  expect_equal(b[2], 0.7142857142857143, 1e-4)
})

test_that("Test SampleEntropy", {
  ta <-
    as.single(c(3, 0, 0, 4, 0, 0, 13))
  tb <-
    as.single(c(3, 0, 0, 4, 0, 0, 13))
  a <- Array(data.frame(ta, tb))
  out <- SampleEntropy(a)
  b <- getData(out)
  expect_equal(b[1], 1.252762968495368, 1e-4)
  expect_equal(b[2], 1.252762968495368, 1e-4)
})

test_that("Test Skewness", {
  ta <-
    as.single(c(3, 0, 0, 4, 0, 0, 13))
  tb <-
    as.single(c(3, 0, 0, 4, 0, 0, 13))
  a <- Array(data.frame(ta, tb))
  out <- Skewness(a)
  b <- getData(out)
  expect_equal(b[1], 2.038404735373753, 1e-4)
  expect_equal(b[2], 2.038404735373753, 1e-4)
})

test_that("Test StandardDeviation", {
  ta <-
    as.single(c(20, 20, 20, 18, 25, 19, 20, 20, 20, 20, 40, 30, 1, 50, 1, 1, 5, 1, 20, 20))
  tb <-
    as.single(c(20, 20, 20, 2, 19, 1, 20, 20, 20, 1, 15, 1, 30, 1, 1, 18, 4, 1, 20, 20))
  a <- Array(data.frame(ta, tb))
  out <- StandardDeviation(a)
  b <- getData(out)
  expect_equal(b[1], 12.363150892875165, 1e-4)
  expect_equal(b[2], 9.51367436903324, 1e-4)
})

test_that("Test SumOfReoccurringDatapoints", {
  ta <-
    as.single(c(3, 3, 0, 4, 0, 13, 13))
  tb <-
    as.single(c(3, 3, 0, 4, 0, 13, 13))
  a <- Array(data.frame(ta, tb))
  out <- SumOfReoccurringDatapoints(a)
  b <- getData(out)
  expect_equal(b[1], 32, 1e-4)
  expect_equal(b[2], 32, 1e-4)
})

test_that("Test SymmetryLooking", {
  ta <-
    as.single(c(20, 20, 20, 18, 25, 19, 20, 20, 20, 20, 40, 30, 1, 50, 1, 1, 5, 1, 20, 20))
  tb <-
    as.single(c(20, 20, 20, 2, 19, 1, 20, 20, 20, 1, 15, 1, 30, 1, 1, 18, 4, 1, 20, 20))
  a <- Array(data.frame(ta, tb))
  out <- SymmetryLooking(a, 0.1)
  b <- getData(out)
  expect_equal(b[1], TRUE)
  expect_equal(b[2], FALSE)
})

test_that("Test ValueCount", {
  ta <-
    as.single(c(20, 20, 20, 18, 25, 19, 20, 20, 20, 20, 40, 30, 1, 50, 1, 1, 5, 1, 20, 20))
  tb <-
    as.single(c(20, 20, 20, 2, 19, 1, 20, 20, 20, 1, 15, 1, 30, 1, 1, 18, 4, 1, 20, 20))
  a <- Array(data.frame(ta, tb))
  out <- ValueCount(a, 20)
  b <- getData(out)
  expect_equal(b[1], 9, 1e-6)
  expect_equal(b[2], 8, 1e-6)
})