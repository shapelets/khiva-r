#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

test_that("Test Lls", {
  SetBackend(4)
  SetDevice(0)
  
  ta <- as.double(c(4, 3))
  tb <- as.double(c(-1, -2))
  blls <-  as.double(c(3, 1))
  
  a <- Array(data.frame(ta, tb))
  clls <- Array(data.frame(blls))
  out <- Lls(a, clls)
  d <- getData(out)
  expect_equal(d[1], 1, 1e-4)
  expect_equal(d[2], 1, 1e-4)
})