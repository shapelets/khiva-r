#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

shared.library <-system.file("extdata","libTSALIB.dylib",package="tsa")
dyn.load(shared.library)
library("bit64")

#' @brief Get the device info.
#' 
#' @export 
Info <- function() {
  
  try(
    out <- .C("info",
              PACKAGE='libTSALIB.dylib')
  )
}

#' @brief Set the back-end.
#' 
#' @export 
SetBackend <- function(backend) {
  
  try(
    out <- .C("set_backend",
              as.integer(backend),
              PACKAGE='libTSALIB.dylib')
  )
}

#' @brief Set the device.
#' 
#' @export 
SetDevice <- function(device) {
  
  try(
    out <- .C("set_device",
              as.integer(device),
              PACKAGE='libTSALIB.dylib')
  )
}