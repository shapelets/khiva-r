#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

test_that("Test Znorm", {
  SetBackend(4)
  SetDevice(0)
  
  ta <-
    as.double(c(0, 1, 2, 3))
  tb <-
    as.double(c(4, 5, 6, 7))
  
  a <- Array(data.frame(ta, tb))
  out <- Znorm(a)
  b <- getData(out)
  expected <-
    as.double(c(
      -1.341640786499870,
      -0.447213595499958,
      0.447213595499958,
      1.341640786499870
    ))
  for (i in 1:4) {
    expect_equal(b[i], expected[i], 1e-3)
    expect_equal(b[i + 4], expected[i], 1e-3)
  }
})