test_that("Test SetBackend", {
  out1 <- GetBackends()
  backends <- out1$result
  cuda <- bitwAnd(backends, 2)
  opencl <- bitwAnd(backends, 4)
  cpu <- bitwAnd(backends, 1)
  
  if (cuda) {
    SetBackend(2)
    out <- GetBackend()
    expect_equal(out$result, 2)
  }
  if (opencl) {
    SetBackend(4)
    out <- GetBackend()
    expect_equal(out$result, 4)
  }
  if (cpu) {
    SetBackend(1)
    out <- GetBackend()
    expect_equal(out$result, 1)
  }
  
})

test_that("Test GetDevice", {
  out1 <- GetBackends()
  backends <- out1$result
  cuda <- bitwAnd(backends, 2)
  opencl <- bitwAnd(backends, 4)
  cpu <- bitwAnd(backends, 1)
  
  if (cuda) {
    SetBackend(2)
    SetDevice(0)
    out <- GetDevice(0)
    expect_equal(out$result, 0)
  }
  if (opencl) {
    SetBackend(4)
    SetDevice(0)
    out <- GetDevice()
    expect_equal(out$result, 0)
    SetDevice(1)
    out <- GetDevice()
    expect_equal(out$result, 1)
  }
  if (cpu) {
    SetBackend(1)
    SetDevice(0)
    out <- GetDevice()
    expect_equal(out$result, 0)
  }
  
})