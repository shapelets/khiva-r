# Copyright (c) 2018 Shapelets.io
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

library(devtools)
install.packages("bit64")
install.packages("testthat")
library(testthat)
install.packages("roxygen2")
library(roxygen2)
devtools::document()
devtools::test()
devtools::install()
install.packages("shiny")
library(shiny)
install.packages("DT")
library(DT)
install.packages("covr")
library("covr")
save.image(file='CIsession.RData')