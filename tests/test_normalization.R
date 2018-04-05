test_that("Test Znorm", {
  SetBackend(4)
  SetDevice(0)
  
  ta <-
    as.double(c(0, 1, 2, 3))
  tb <-
    as.double(c(4, 5, 6, 7))
  out <- Znorm(list(ta, tb))
  expected <-
    as.double(c(
      -1.341640786499870,
      -0.447213595499958,
      0.447213595499958,
      1.341640786499870
    ))
  for (i in 1:4) {
    expect_equal(out[i], expected[i])
    expect_equal(out[i + 4], expected[i])
  }
})