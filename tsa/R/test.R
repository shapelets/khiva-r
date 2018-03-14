#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.
#
ta <- as.double(c(0, 1, 2, 3, 4, 5))
tb <- as.double(c(6, 7, 8, 9, 10, 11))
library(bit64)
m  <- as.integer64(3)
out <- Stomp(ta, tb, m)
out1 <- StompSelfJoin(tb, m)
out2 <- FindBestNMotifs(out$profile, out$index, 3)
out3 <- FindBestNDiscords(out$profile, out$index, 3)
out4 <- AbsoluteSumOfChanges(list(ta, tb))
out5 <- AbsEnergy((list(ta, tb)))
out6 <- C3((list(ta, tb)), 2)
out7 <- CidCe((list(ta, tb)), FALSE)