#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

platform <- Sys.info()['sysname']

if (platform == 'Darwin') {
  shared.library <-
    system.file("extdata", "libtsa_c.dylib", package = "tsa")
  dyn.load(shared.library)
  library <- "libtsa_c.dylib"
}
if (platform == 'Windows') {
  shared.library <-
    system.file("extdata", "libtsa_c.dll", package = "tsa")
  dyn.load(shared.library)
  library <- "libtsa_c.dll"
}
if (platform == 'Linux') {
  shared.library <-
    system.file("extdata", "libtsa_c.so", package = "tsa")
  dyn.load(shared.library)
  library <- "libtsa_c.so"
}

library("bit64")

TSABackend <- function() {
  list(
    TSA_BACKEND_DEFAULT = 0,
    TSA_BACKEND_CPU = 1,
    TSA_BACKEND_CUDA = 2,
    TSA_BACKEND_OPENCL = 4
  )
}

TSABackendFromOrdinal <- function(number) {
  switch(
    toString(number),
    "0" = {
      return(TSABackend()[1])
    },
    "1" = {
      return(TSABackend()[2])
    },
    "2" = {
      return(TSABackend()[3])
    },
    "4" = {
      return(TSABackend()[4])
    }
  )
}

#' Info
#' 
#' Get the device info.
#'
#' @export
Info <- function() {
  try(out <- .C("info",
                PACKAGE = library))
}

#' SetBackend
#' 
#' Set the backend.
#'
#' @export
SetBackend <- function(backend) {
  try(out <- .C("set_backend",
                as.integer(backend),
                PACKAGE = library))
}

#' GetBackend
#' 
#' Get the active backend.
#'
#' @return The active backend.
#' @export
GetBackend <- function() {
  try(out <- .C("get_backend",
                result = as.integer(seq(
                  length = 1,
                  from = 0,
                  to = 0
                )),
                PACKAGE = library))
  
  return(TSABackendFromOrdinal(out$result))
}

#' GetBackends
#' 
#' Get the active available backends.
#'
#' @return The available backends.
#' @export
GetBackends <- function() {
  try(out <- .C("get_backends",
                result = as.integer(seq(
                  length = 1,
                  from = 0,
                  to = 0
                )),
                PACKAGE = library))
  
  newList <- list("result" = out$result)
  
  return(newList)
}

#' SetDevice
#' 
#' Set the device.
#'
#' @param device The desired device.
#' @export
SetDevice <- function(device) {
  try(out <- .C("set_device",
                as.integer(device),
                PACKAGE = library))
}

#' GetDeviceID
#' 
#' Get the device id.
#'
#' @param device The active device.
#' @export
GetDeviceID <- function() {
  try(out <- .C("get_device_id",
                result = as.integer(seq(
                  length = 1,
                  from = 0,
                  to = 0
                )),
                PACKAGE = library))
  newList <- list("result" = out$result)
  
  return(newList)
}

#' GetDeviceCount
#'
#' Get the devices count
#'
#' @return The devices count.
#' @export
GetDeviceCount <- function() {
  try(out <- .C("get_device_count",
                result = as.integer(seq(
                  length = 1,
                  from = 0,
                  to = 0
                )),
                PACKAGE = library))
  newList <- list("result" = out$result)
  
  return(newList)
}