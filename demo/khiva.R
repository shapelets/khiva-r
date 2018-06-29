# Copyright (c) 2018 Shapelets.io
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

# Creation of a khiva Array
ta <- as.double(c(1, 2, 3, 4, 5, 6, 7, 8))
a <- Array(array(ta, dim=c(8,1)))
# Displaying a khiva Array
display(a)
# Gets an R array from a khiva Array
d <- getData(a)
print(d)
# Realease the khiva Array
deleteArray(a)