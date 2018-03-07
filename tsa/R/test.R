ta <- as.double(c(1, 2, 3, 5, 6, 7, 8, 10))
tb <- as.double(c(4, 5, 6, 24, 24, 24, 3, 3))
library(bit64)
m  <- as.integer64(3)
out <- stomp(ta,tb,m)
out1 <- stomp_self_join(tb,m)

