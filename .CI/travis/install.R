# Copyright (c) 2018 Shapelets.io
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

list.of.packages <- c("devtools", "bit64", "testthat", "roxygen2", "shiny", "DT", "covr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="https://cloud.r-project.org/")

Packages <- c("devtools", "bit64", "testthat", "roxygen2", "shiny", "DT", "covr", "git2r")
lapply(Packages, library, character.only = TRUE)

save.image(file='CIsession.RData')
