#
#Copyright (c) 2018 Shapelets.io
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

library(tidyverse)
library(httr)
library(jsonlite)

testthat::setup(
  SetBackend(KHIVABackend()$KHIVA_BACKEND_CPU)
)

context("Khiva SetBackend tests")

test_that("Test SetBackend", {
  prev.backend <- GetBackend()
  prev.device <- GetDeviceID()
  out1 <- GetBackends()
  backends <- out1$result
  cuda <- bitwAnd(backends, KHIVABackend()$KHIVA_BACKEND_CUDA)
  opencl <- bitwAnd(backends, KHIVABackend()$KHIVA_BACKEND_OPENCL)
  cpu <- bitwAnd(backends, KHIVABackend()$KHIVA_BACKEND_CPU)
  
  if (cuda) {
    SetBackend(KHIVABackend()$KHIVA_BACKEND_CUDA)
    out <- GetBackend()
    expect_equal(out[[1]], KHIVABackend()$KHIVA_BACKEND_CUDA)
  }
  if (opencl) {
    SetBackend(KHIVABackend()$KHIVA_BACKEND_OPENCL)
    out <- GetBackend()
    expect_equal(out[[1]], KHIVABackend()$KHIVA_BACKEND_OPENCL)
  }
  if (cpu) {
    SetBackend(KHIVABackend()$KHIVA_BACKEND_CPU)
    out <- GetBackend()
    expect_equal(out[[1]], KHIVABackend()$KHIVA_BACKEND_CPU)
  }
  
  SetBackend(prev.backend)
  SetDevice(prev.device)
})

context("Khiva GetDeviceID tests")

test_that("Test GetDeviceID", {
  prev.backend <- GetBackend()
  prev.device <- GetDeviceID()
  out1 <- GetBackends()
  backends <- out1$result
  cuda <- bitwAnd(backends, KHIVABackend()$KHIVA_BACKEND_CUDA)
  opencl <- bitwAnd(backends, KHIVABackend()$KHIVA_BACKEND_OPENCL)
  cpu <- bitwAnd(backends, KHIVABackend()$KHIVA_BACKEND_CPU)
  
  if (cuda) {
    SetBackend(KHIVABackend()$KHIVA_BACKEND_CUDA)
    device.count <- GetDeviceCount()
    for (i in 0:(device.count$result - 1)) {
      SetDevice(i)
      out <- GetDeviceID()
      expect_equal(out$result, i)
    }
  }
  if (opencl) {
    SetBackend(KHIVABackend()$KHIVA_BACKEND_OPENCL)
    device.count <- GetDeviceCount()
    for (i in 0:(device.count$result - 1)) {
      SetDevice(i)
      out <- GetDeviceID()
      expect_equal(out$result, i)
    }
  }
  if (cpu) {
    SetBackend(KHIVABackend()$KHIVA_BACKEND_CPU)
    device.count <- GetDeviceCount()
    for (i in 0:(device.count$result - 1)) {
      SetDevice(i)
      out <- GetDeviceID()
      expect_equal(out$result, i)
    }
  }
  
  SetBackend(prev.backend)
  SetDevice(prev.device)
})

context("Khiva Version tests")

test_that("Test Version", {
  out <- Version()
  
  tags <- GET(url = 'https://api.github.com/repos/shapelets/khiva/tags')
  content <- content(tags)
  tag <- content[length(content)]
  version <- tag[[1]]$name
  version <- str_replace(version, 'v', '')
  version <- str_replace(version, '-RC', '')
  
  expect_equal(out$result, version)
})