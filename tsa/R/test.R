#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.
#
ta <- as.double(c(1, 2, 3, 5, 6, 7, 8, 10))
tb <- as.double(c(4, 5, 6, 24, 24, 24, 3, 3))
library(bit64)
m  <- as.integer64(3)
out <- stomp(ta,tb,m)
out1 <- stomp_self_join(tb,m)

