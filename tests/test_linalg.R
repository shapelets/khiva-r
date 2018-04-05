test_that("Test Lls", {
  SetBackend(4)
  SetDevice(0)
  
  ta <-
    as.double(c(4, 3))
  tb <-
    as.double(c(-1, -2))
  b <-  as.double(c(3, 1))
  out <- Lls(list(ta, tb), b)
  expect_equal(out[1], 1, 1e-4)
  expect_equal(out[2], 1, 1e-4)
})