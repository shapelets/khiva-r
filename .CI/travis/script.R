# Copyright (c) 2018 Shapelets.io
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

load('CIsession.RData')
library("methods")
library(devtools)
install_github("git2r", "ropensci")
devtools::document()
devtools::install()
devtools::test()
covr::to_cobertura(covr::package_coverage())
