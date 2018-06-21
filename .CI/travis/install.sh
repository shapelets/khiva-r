#!/bin/bash
# Copyright (c) 2018 Shapelets.io
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

if [[ "$TRAVIS_OS_NAME" == "osx" ]]; then
    Rscript .CI/travis/install-osx.R
else
    apt-get update
    apt-get install libxml2-dev libxslt-dev -y
    Rscript .CI/travis/install-linux.R
fi