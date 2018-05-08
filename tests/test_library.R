#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

test_that("Test SetBackend", {
  out1 <- GetBackends()
  backends <- out1$result
  cuda <- bitwAnd(backends, TSABackend()$TSA_BACKEND_CUDA)
  opencl <- bitwAnd(backends, TSABackend()$TSA_BACKEND_OPENCL)
  cpu <- bitwAnd(backends, TSABackend()$TSA_BACKEND_CPU)
  
  if (cuda) {
    SetBackend(TSABackend()$TSA_BACKEND_CUDA)
    out <- GetBackend()
    expect_equal(out[[1]], TSABackend()$TSA_BACKEND_CUDA)
  }
  if (opencl) {
    SetBackend(TSABackend()$TSA_BACKEND_OPENCL)
    out <- GetBackend()
    expect_equal(out[[1]], TSABackend()$TSA_BACKEND_OPENCL)
  }
  if (cpu) {
    SetBackend(TSABackend()$TSA_BACKEND_CPU)
    out <- GetBackend()
    expect_equal(out[[1]], TSABackend()$TSA_BACKEND_CPU)
  }
})

test_that("Test GetDeviceID", {
  out1 <- GetBackends()
  backends <- out1$result
  cuda <- bitwAnd(backends, TSABackend()$TSA_BACKEND_CUDA)
  opencl <- bitwAnd(backends, TSABackend()$TSA_BACKEND_OPENCL)
  cpu <- bitwAnd(backends, TSABackend()$TSA_BACKEND_CPU)
  
  if (cuda) {
    SetBackend(TSABackend()$TSA_BACKEND_CUDA)
    device.count <- GetDeviceCount()
    for (i in 0:(device.count$result - 1)) {
      SetDevice(i)
      out <- GetDeviceID()
      expect_equal(out$result, i)
    }
  }
  if (opencl) {
    SetBackend(TSABackend()$TSA_BACKEND_OPENCL)
    device.count <- GetDeviceCount()
    for (i in 0:(device.count$result - 1)) {
      SetDevice(i)
      out <- GetDeviceID()
      expect_equal(out$result, i)
    }
  }
  if (cpu) {
    SetBackend(TSABackend()$TSA_BACKEND_CPU)
    device.count <- GetDeviceCount()
    for (i in 0:(device.count$result - 1)) {
      SetDevice(i)
      out <- GetDeviceID()
      expect_equal(out$result, i)
    }
  }
})

test_that("Test Version", {
  out <- Version()
  expect_equal(out$result, "0.0.1")
})