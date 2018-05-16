#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

test_that("Test Linear", {
  ta <-
    as.single(
      c(
        0.24580423,
        0.59642861,
        0.35879163,
        0.37891011,
        0.02445137,
        0.23830957,
        0.38793433,
        0.68054104,
        0.83934083,
        0.76073689
      )
    )
  a <- Array(array(ta, dim = c(10, 1)))
  tb <-
    as.single(
      c(
        0.2217416,
        0.06344161,
        0.77944375,
        0.72174137,
        0.19413884,
        0.51146167,
        0.06880307,
        0.39414268,
        0.98172767,
        0.30490851
      )
    )
  b <- Array(array(tb, dim = c(10, 1)))
  out <- Linear(a, b)
  slope <- c(getData(out$slope))
  intercept <- c(getData(out$intercept))
  rvalue <- c(getData(out$rvalue))
  pvalue <- c(getData(out$pvalue))
  stderrest <- c(getData(out$stderrest))
  
  expect_equal(slope[1], 0.344864266, 1e-6)
  expect_equal(intercept[1], 0.268578232, 1e-6)
  expect_equal(rvalue[1], 0.283552942, 1e-6)
  expect_equal(pvalue[1], 0.427239418, 1e-6)
  expect_equal(stderrest[1], 0.412351891, 1e-6)
  
  deleteArray(a)
  deleteArray(b)
  
  deleteArray(out$slope)
  deleteArray(out$intercept)
  deleteArray(out$rvalue)
  deleteArray(out$pvalue)
  deleteArray(out$stderrest)
})

test_that("Test LinearMultipleTimeSeries", {
  ta <-
    as.single(
      c(
        0.24580423,
        0.59642861,
        0.35879163,
        0.37891011,
        0.02445137,
        0.23830957,
        0.38793433,
        0.68054104,
        0.83934083,
        0.76073689,
        0.24580423,
        0.59642861,
        0.35879163,
        0.37891011,
        0.02445137,
        0.23830957,
        0.38793433,
        0.68054104,
        0.83934083,
        0.76073689
      )
    )
  a <- Array(array(ta, dim = c(10, 2)))
  tb <-
    as.single(
      c(
        0.2217416,
        0.06344161,
        0.77944375,
        0.72174137,
        0.19413884,
        0.51146167,
        0.06880307,
        0.39414268,
        0.98172767,
        0.30490851,
        0.2217416,
        0.06344161,
        0.77944375,
        0.72174137,
        0.19413884,
        0.51146167,
        0.06880307,
        0.39414268,
        0.98172767,
        0.30490851
      )
    )
  b <- Array(array(tb, dim = c(10, 2)))
  out <- Linear(a, b)
  slope <- c(getData(out$slope))
  intercept <- c(getData(out$intercept))
  rvalue <- c(getData(out$rvalue))
  pvalue <- c(getData(out$pvalue))
  stderrest <- c(getData(out$stderrest))
  
  expect_equal(slope[1], 0.344864266, 1e-6)
  expect_equal(intercept[1], 0.268578232, 1e-6)
  expect_equal(rvalue[1], 0.283552942, 1e-6)
  expect_equal(pvalue[1], 0.427239418, 1e-6)
  expect_equal(stderrest[1], 0.412351891, 1e-6)
  
  expect_equal(slope[2], 0.344864266, 1e-6)
  expect_equal(intercept[2], 0.268578232, 1e-6)
  expect_equal(rvalue[2], 0.283552942, 1e-6)
  expect_equal(pvalue[2], 0.427239418, 1e-6)
  expect_equal(stderrest[2], 0.412351891, 1e-6)
  
  deleteArray(a)
  deleteArray(b)
  
  deleteArray(out$slope)
  deleteArray(out$intercept)
  deleteArray(out$rvalue)
  deleteArray(out$pvalue)
  deleteArray(out$stderrest)
})