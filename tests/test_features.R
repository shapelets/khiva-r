#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

test_that("Test C3", {
  ta <- as.double(c(0, 1, 2, 3, 4, 5))
  tb <- as.double(c(6, 7, 8, 9, 10, 11))
  out <- C3((list(ta, tb)), 2)
  
  expect_equal(out$result[1], 7.5)
  expect_equal(out$result[2], 586.5)
})

test_that("Test CidCe", {
  ta <- as.double(c(0, 1, 2, 3, 4, 5))
  tb <- as.double(c(6, 7, 8, 9, 10, 11))
  out <- CidCe((list(ta, tb)), TRUE)
  
  expect_equal(out$result[1], 1.30930734141595)
  expect_equal(out$result[2], 1.30930734141595)
  
  out <- CidCe((list(ta, tb)), FALSE)
  expect_equal(out$result[1], 2.23606797749979)
  expect_equal(out$result[2], 2.23606797749979)
})

test_that("Test AbsEnergy", {
  ta <- as.double(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  out <- AbsEnergy(list(ta))
  
  expect_equal(out$result[1], 385)
})

test_that("Test AbsoluteSumOfChanges", {
  ta <- as.double(c(0, 1, 2, 3))
  tb <- as.double(c(4, 6, 8, 10))
  tc <- as.double(c(11, 14, 17, 20))
  out <- AbsoluteSumOfChanges(list(ta, tb, tc))
  
  expect_equal(out$result[1], 3)
  expect_equal(out$result[2], 6)
  expect_equal(out$result[3], 9)
})

test_that("Test CrossCorrelation", {
  ta <- as.double(c(1, 2, 3, 4))
  tb <- as.double(c(4, 6, 8, 10, 12))
  out <- CrossCorrelation(list(ta), list(tb), FALSE)
  
  expect_equal(out$result[1], 0.790569415)
  expect_equal(out$result[2], 0.790569415)
  expect_equal(out$result[3], 0.079056941)
  expect_equal(out$result[4], -0.395284707)
  expect_equal(out$result[5], -0.474341649)
})

test_that("Test AutoCovariance", {
  ta <- as.double(c(0, 1, 2, 3))
  tb <- as.double(c(10, 11, 12, 13))
  out <- AutoCovariance(list(ta, tb), FALSE)
  
  expect_equal(out$result[1], 1.25)
  expect_equal(out$result[2], 0.3125)
  expect_equal(out$result[3], -0.375)
  expect_equal(out$result[4], -0.5625)
  expect_equal(out$result[5], 1.25)
  expect_equal(out$result[6], 0.3125)
  expect_equal(out$result[7], -0.375)
  expect_equal(out$result[8], -0.5625)
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
    expect_equal(out$result[(i * 5) + 4], -1.25)
    expect_equal(out$result[(i * 5) + 5], -1.5)
  }
})

test_that("Test ApproximateEntropy", {
  ta <- as.double(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  tb <- as.double(c(11, 12, 13, 14, 15, 16, 17, 18, 19, 20))
  
  out <- ApproximateEntropy(list(ta, tb), 4, 0.5)
  expect_equal(out$result[1], 0.13484275341033936, 1e-6)
  expect_equal(out$result[2], 0.13484275341033936, 1e-6)
})

test_that("Test AutoCorrelation", {
  ta <- as.double(c(0, 1, 2, 3))
  tb <- as.double(c(10, 11, 12, 13))
  
  out <- AutoCorrelation(list(ta, tb), 4, FALSE)
  expect_equal(out[1], 1, 1e-6)
  expect_equal(out[2], 0.25, 1e-6)
  expect_equal(out[3],-0.3, 1e-6)
  expect_equal(out[4],-0.45, 1e-6)
  expect_equal(out[5], 1, 1e-6)
  expect_equal(out[6], 0.25, 1e-6)
  expect_equal(out[7],-0.3, 1e-6)
  expect_equal(out[8],-0.45, 1e-6)
})

test_that("Test BinnedEntropy", {
  ta <- as.double(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
                    14, 15, 16, 17, 18, 19, 20))
  tb <-
    as.double(c(1, 1, 3, 10, 5, 6, 1, 8, 9, 10, 11, 1, 13, 14, 10, 16, 17, 10, 19,
                20))
  
  out <- BinnedEntropy(list(ta, tb), 5)
  expect_equal(out[1], 1.6094379124341005, 1e-6)
  expect_equal(out[2], 1.5614694247763998, 1e-6)
})

test_that("Test CountAboveMean", {
  ta <- as.double(c(0, 1, 2, 3, 4, 5))
  tb <- as.double(c(6, 7, 8, 9, 10, 11))
  
  out <- CountAboveMean(list(ta, tb))
  expect_equal(out[1], 3)
  expect_equal(out[2], 3)
})

test_that("Test CountBelowMean", {
  ta <- as.double(c(0, 1, 2, 3, 4, 5))
  tb <- as.double(c(6, 7, 8, 9, 10, 11))
  
  out <- CountBelowMean(list(ta, tb))
  expect_equal(out[1], 3)
  expect_equal(out[2], 3)
})

test_that("Test EnergyRatioBychunks", {
  ta <- as.double(c(0, 1, 2, 3, 4, 5))
  tb <- as.double(c(6, 7, 8, 9, 10, 11))
  
  out <- EnergyRatioByChunks(list(ta, tb), 2, 0)
  expect_equal(out[1], 0.090909091)
  expect_equal(out[2], 0.330376940)
  
  out <- EnergyRatioByChunks(list(ta, tb), 2, 1)
  expect_equal(out[1], 0.909090909)
  expect_equal(out[2], 0.669623060)
})

test_that("Test CountBelowMean", {
  ta <- as.double(c(0, 1, 2, 3, 4, 5))
  tb <- as.double(c(6, 7, 8, 9, 10, 11))
  
  out <- CountBelowMean(list(ta, tb))
  expect_equal(out[1], 3)
  expect_equal(out[2], 3)
})

test_that("Test FirstLocationOfMaximum", {
  ta <- as.double(c(5, 4, 3, 5, 0, 1, 5, 3, 2, 1))
  tb <- as.double(c(2, 4, 3, 5, 2, 5, 4, 3, 5, 2))
  
  out <- FirstLocationOfMaximum(list(ta, tb))
  expect_equal(out[1], 0.0)
  expect_equal(out[2], 0.3)
})

test_that("Test FirstLocationOfMinimum", {
  ta <- as.double(c(5, 4, 3, 0, 0, 1))
  tb <- as.double(c(5, 4, 3, 0, 2, 1))
  
  out <- FirstLocationOfMinimum(list(ta, tb))
  expect_equal(out[1], 0.5)
  expect_equal(out[2], 0.5)
})

test_that("Test HasDuplicates", {
  ta <- as.double(c(5, 4, 3, 0, 0, 1))
  tb <- as.double(c(5, 4, 3, 0, 2, 1))
  
  out <- HasDuplicates(list(ta, tb))
  expect_equal(out[1], TRUE)
  expect_equal(out[2], FALSE)
})

test_that("Test HasDuplicateMax", {
  ta <- as.double(c(5, 4, 3, 0, 5, 1))
  tb <- as.double(c(5, 4, 3, 0, 2, 1))
  
  out <- HasDuplicateMax(list(ta, tb))
  expect_equal(out[1], TRUE)
  expect_equal(out[2], FALSE)
})

test_that("Test IndexMaxQuantile", {
  ta <- as.double(c(5, 4, 3, 0, 5, 1))
  tb <- as.double(c(5, 4, 3, 0, 2, 1))
  
  out <- IndexMaxQuantile(list(ta, tb), 0.5)
  expect_equal(out[1], 0.333333333)
  expect_equal(out[2], 0.333333333)
})

test_that("Test Kurtosis", {
  ta <- as.double(c(0, 1, 2, 3, 4, 5))
  tb <- as.double(c(2, 2, 2, 20, 30, 25))
  
  out <- Kurtosis(list(ta, tb))
  expect_equal(out[1], -1.2)
  expect_equal(out[2], -2.66226722)
})

test_that("Test LargeStandardDeviation", {
  ta <- as.double(c(-1, -1, -1, 1, 1, 1))
  tb <- as.double(c(4, 6, 8, 4, 5, 4))
  
  out <- LargeStandardDeviation(list(ta, tb), 0.4)
  expect_equal(out[1], TRUE)
  expect_equal(out[2], FALSE)
})

test_that("Test LastLocationOfMaximum", {
  ta <- as.double(c(0, 4, 3, 5, 5, 1))
  tb <- as.double(c(0, 4, 3, 2, 5, 1))
  
  out <- LastLocationOfMaximum(list(ta, tb))
  expect_equal(out[1], 0.8333333333333334)
  expect_equal(out[2], 0.8333333333333334)
})

test_that("Test LastLocationOfMinimum", {
  ta <- as.double(c(0, 4, 3, 5, 5, 1, 0, 4))
  tb <- as.double(c(3, 2, 5, 1, 4, 5, 1, 2))
  
  out <- LastLocationOfMinimum(list(ta, tb))
  expect_equal(out[1], 0.875)
  expect_equal(out[2], 0.875)
})

test_that("Test Length", {
  ta <- as.double(c(0, 4, 3, 5, 5, 1))
  tb <- as.double(c(0, 4, 3, 2, 5, 1))
  
  out <- Length(list(ta, tb))
  expect_equal(out[1], 6)
  expect_equal(out[2], 6)
})

test_that("Test LinearTrend", {
  ta <- as.double(c(0, 4, 3, 5, 5, 1))
  tb <- as.double(c(2, 4, 1, 2, 5, 3))
  
  out <- LinearTrend(list(ta, tb))
  expect_equal(out$pvalue[1], 0.6260380997892747, 1e-6)
  expect_equal(out$pvalue[2], 0.5272201945463578, 1e-6)
  
  expect_equal(out$rvalue[1], 0.2548235957188128, 1e-6)
  expect_equal(out$rvalue[2], 0.3268228676411533, 1e-6)
  
  expect_equal(out$intercept[1], 2.2857142857142856, 1e-6)
  expect_equal(out$intercept[2], 2.1904761904761907, 1e-6)
  
  expect_equal(out$slope[1], 0.2857142857142857, 1e-6)
  expect_equal(out$slope[2], 0.2571428571428572, 1e-6)
  
  expect_equal(out$stdrr[1], 0.5421047417431507, 1e-6)
  expect_equal(out$stdrr[2], 0.37179469135129783, 1e-6)
})

test_that("Test HasDuplicateMin", {
  ta <- as.double(c(5, 4, 3, 0, 0, 1))
  tb <- as.double(c(5, 4, 3, 0, 2, 1))
  
  out <- HasDuplicateMin(list(ta, tb))
  expect_equal(out[1], TRUE)
  expect_equal(out[2], FALSE)
})

test_that("Test LongestStrikeAboveMean", {
  ta <-
    as.double(c(20, 20, 20, 1, 1, 1, 20, 20, 20, 20, 1, 1, 1, 1, 1, 1, 1, 1, 20, 20))
  tb <-
    as.double(c(20, 20, 20, 1, 1, 1, 20, 20, 20, 1,  1, 1, 1, 1, 1, 1, 1, 1, 20, 20))
  
  out <- LongestStrikeAboveMean(list(ta, tb))
  expect_equal(out[1], 4, 1e-6)
  expect_equal(out[2], 3, 1e-6)
})

test_that("Test LongestStrikeBelowMean", {
  ta <-
    as.double(c(20, 20, 20, 1, 1, 1, 20, 20, 20, 20, 1, 1, 1, 1, 1, 1, 1, 1, 20, 20))
  tb <-
    as.double(c(20, 20, 20, 1, 1, 1, 20, 20, 20, 1,  1, 1, 1, 1, 1, 1, 1, 1, 20, 20))
  
  out <- LongestStrikeBelowMean(list(ta, tb))
  expect_equal(out[1], 8, 1e-6)
  expect_equal(out[2], 9, 1e-6)
})

test_that("Test Maximum", {
  ta <-
    as.double(c(20, 20, 20, 18, 25, 19, 20, 20, 20, 20, 40, 30, 1,  50, 1, 1,  5, 1, 20, 20))
  tb <-
    as.double(c(20, 20, 20, 2,  19, 1,  20, 20, 20, 1,  15, 1,  30, 1,  1, 18, 4, 1, 20, 20))
  
  out <- Maximum(list(ta, tb))
  expect_equal(out[1], 50, 1e-6)
  expect_equal(out[2], 30, 1e-6)
})

test_that("Test MeanAbsoluteChange", {
  ta <- as.double(c(0, 1, 2, 3, 4, 5))
  tb <- as.double(c(8, 10, 12, 14, 16, 18))
  
  out <- MeanAbsoluteChange(list(ta, tb))
  expect_equal(out[1], 5 / 6, 1e-6)
  expect_equal(out[2], 10 / 6, 1e-6)
})

test_that("Test FftCoefficient", {
  ta <- as.double(c(0, 1, 2, 3, 4, 5))
  tb <- as.double(c(6, 7, 8, 9, 10, 11))
  
  out <- FftCoefficient(list(ta, tb), 0)
  expect_equal(out$real[1], 15, 1e-6)
  expect_equal(out$real[2], 51, 1e-6)
  
  expect_equal(out$imag[1], 0, 1e-6)
  expect_equal(out$imag[2], 0, 1e-6)
  
  expect_equal(out$abs[1], 15, 1e-6)
  expect_equal(out$abs[2], 51, 1e-6)
  
  expect_equal(out$angle[1], 0, 1e-6)
  expect_equal(out$angle[2], 0, 1e-6)
})

test_that("Test AggregatedAutocorrelationMean", {
  ta <- as.double(c(1, 2, 3, 4, 5, 6))
  tb <- as.double(c(7, 8, 9, 10, 11, 12))
  
  out <- AggregatedAutocorrelation(list(ta, tb), 0)
  expect_equal(out[1], -0.6571428571428571, 1e-6)
  expect_equal(out[2], -0.6571428571428571, 1e-6)
})

test_that("Test AggregatedAutocorrelationMedian", {
  ta <- as.double(c(1, 2, 3, 4, 5, 6))
  tb <- as.double(c(7, 8, 9, 10, 11, 12))
  
  out <- AggregatedAutocorrelation(list(ta, tb), 1)
  expect_equal(out[1], -0.54285717010498047, 1e-6)
  expect_equal(out[2], -0.54285717010498047, 1e-6)
})

test_that("Test AggregatedAutocorrelationMin", {
  ta <- as.double(c(1, 2, 3, 4, 5, 6))
  tb <- as.double(c(7, 8, 9, 10, 11, 12))
  
  out <- AggregatedAutocorrelation(list(ta, tb), 2)
  expect_equal(out[1], -2.142857142857143, 1e-6)
  expect_equal(out[2], -2.142857142857143, 1e-6)
})

test_that("Test AggregatedAutocorrelationMax", {
  ta <- as.double(c(1, 2, 3, 4, 5, 6))
  tb <- as.double(c(7, 8, 9, 10, 11, 12))
  
  out <- AggregatedAutocorrelation(list(ta, tb), 3)
  expect_equal(out[1], 0.6, 1e-6)
  expect_equal(out[2], 0.6, 1e-6)
})

test_that("Test AggregatedAutocorrelationStdev", {
  ta <- as.double(c(1, 2, 3, 4, 5, 6))
  tb <- as.double(c(7, 8, 9, 10, 11, 12))
  
  out <- AggregatedAutocorrelation(list(ta, tb), 4)
  expect_equal(out[1], 0.9744490855905009, 1e-6)
  expect_equal(out[2], 0.9744490855905009, 1e-6)
})

test_that("Test AggregatedAutocorrelationVar", {
  ta <- as.double(c(1, 2, 3, 4, 5, 6))
  tb <- as.double(c(7, 8, 9, 10, 11, 12))
  
  out <- AggregatedAutocorrelation(list(ta, tb), 5)
  expect_equal(out[1], 0.9495510204081633, 1e-6)
  expect_equal(out[2], 0.9495510204081633, 1e-6)
})

test_that("Test AggregatedLinearTrendMean", {
  ta <- as.double(c(2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5))
  
  out <- AggregatedLinearTrend(list(ta), 3, 0)
  expect_equal(out$slope[1], 1, 1e-6)
  
  expect_equal(out$intercept[1], 2, 1e-6)
  
  expect_equal(out$rvalue[1], 1, 1e-6)
  
  expect_equal(out$pvalue[1], 0, 1e-6)
  
  expect_equal(out$stderrest[1], 0, 1e-6)
})

test_that("Test AggregatedLinearTrendMin", {
  ta <- as.double(c(2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5))
  
  out <- AggregatedLinearTrend(list(ta), 3, 2)
  expect_equal(out$slope[1], 1, 1e-6)
  
  expect_equal(out$intercept[1], 2, 1e-6)
  
  expect_equal(out$rvalue[1], 1, 1e-6)
  
  expect_equal(out$pvalue[1], 0, 1e-6)
  
  expect_equal(out$stderrest[1], 0, 1e-6)
})

test_that("Test CwtCoefficients", {
  ta <- as.double(c(0.1, 0.2, 0.3))
  tb <- as.double(c(0.1, 0.2, 0.3))
  
  w <- as.integer(c(1, 2, 3))
  out <- CwtCoefficients(list(ta, tb), list(w), 2, 2)
  expect_equal(out[1], 0.26517161726951599, 1e-6)
  expect_equal(out[2], 0.26517161726951599, 1e-6)
})

test_that("Test MeanSecondDerivativeCentral", {
  ta <- as.double(c(1, 3, 7, 4, 8))
  tb <- as.double(c(2, 5, 1, 7, 4))
  
  out <- MeanSecondDerivativeCentral(list(ta, tb))
  expect_equal(out[1], as.double(1.0 / 5.0), 1e-6)
  expect_equal(out[2], as.double(-3.0 / 5.0), 1e-6)
})

test_that("Test Minimum", {
  ta <-
    as.double(c(
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
    as.double(c(20, 20, 20, 2, 19, 4, 20, 20, 20, 4, 15, 6, 30, 7, 9, 18, 4, 10, 20, 20))
  
  out <- Minimum(list(ta, tb))
  expect_equal(out[1], 1, 1e-6)
  expect_equal(out[2], 2, 1e-6)
})

test_that("Test NumberCrossingM", {
  ta <-
    as.double(c(1, 2, 1, 1, -3, -4, 7, 8, 9, 10, -2, 1, -3, 5, 6, 7, -10))
  tb <-
    as.double(c(1, 2, 1, 1, -3, -4, 7, 8, 9, 10, -2, 1, -3, 5, 6, 7, -10))
  
  out <- NumberCrossingM(list(ta, tb), 0)
  expect_equal(out[1], 7, 1e-6)
  expect_equal(out[2], 7, 1e-6)
})