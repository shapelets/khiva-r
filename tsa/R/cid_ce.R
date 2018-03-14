#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.
#
#
#' @brief cidCe function.
#' 
#' @param tss List of time series double arrays.
#' @param z.normalize Controls whether the time series should be z-normalized or not.
#' @return The complexity value for the given time series.
#' @export 

CidCe <- function(tss, z.normalize) {
  
  shared.library <- system.file("extdata", "libTSALIB.dylib", package="tsa")
  dyn.load(shared.library, PACKAGE = 'libTSALIB.dylib')
  
  library("bit64")
  
  tss.length <- as.integer64(length(tss[[1]]))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  tss.number.of.ts <- as.integer64(length(tss))
  
  try(
    out <- .C("cidCe",
              tss.concatenated,
              tss.length,
              tss.number.of.ts,
              z.normalize,
              result = as.double(seq(length = tss.number.of.ts), from = 0, to = 0),
              PACKAGE='libTSALIB.dylib')
  )
  
  r.result <- list("result" = out$result)
  
  return(r.result)
}