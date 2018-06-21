# Copyright (c) 2018 Shapelets.io
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
install.packages("devtools", repos="https://cloud.r-project.org/")
library(devtools)
install.packages("bit64", repos="https://cloud.r-project.org/")
install.packages("testthat", repos="https://cloud.r-project.org/")
library(testthat)
install.packages("roxygen2", repos="https://cloud.r-project.org/")
library(roxygen2)
library(methods)
save.image(file='CIsession.RData')