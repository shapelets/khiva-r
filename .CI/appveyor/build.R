# Copyright (c) 2019 Shapelets.io
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

load('CIsession.RData')
devtools::document()
options(devtools.install.args = "--no-multiarch")
devtools::install()
save.image(file='CIsession.RData')
