#
#Copyright (c) 2018 Shapelets.io
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

PackageName <- function() {
  platform <- Sys.info()['sysname']

  if (platform == 'Darwin') {
    package <- 'libkhiva_c.dylib'
  }
  else if (platform == 'Windows') {
    package <- 'khiva_c'
  }
  else if (platform == 'Linux') {
    package <- 'libkhiva_c'
  }

  return(package)
}

LoadLibraries <- function() {
  platform <- Sys.info()['sysname']

  if (platform == 'Darwin') {
    shared.library <- '/usr/local/lib/libkhiva_c.dylib'
    if(!is.null(getLoadedDLLs()$libkhiva_c[[3]])){
      dyn.unload(shared.library)
      dyn.unload('/usr/local/lib/libaf.3.dylib')
    }
    dyn.load('/usr/local/lib/libaf.3.dylib', local = FALSE)
    dyn.load(shared.library, local = FALSE)
  }
  else if (platform == 'Windows') {
    shared.library <- 'C:\\Program Files\\Khiva\\v0\\lib\\khiva_c.dll'
    if(!is.null(getLoadedDLLs()$khiva_c[[3]])){
      dyn.unload('C:\\Program Files\\ArrayFire\\v3\\lib\\af.dll')
      dyn.unload(shared.library)
    }
    dyn.load('C:\\Program Files\\ArrayFire\\v3\\lib\\af.dll', local = FALSE)
    dyn.load(shared.library, local = FALSE)
  }
  else if (platform == 'Linux') {
    shared.library <- '/usr/local/lib/libkhiva_c.so'
    if(!is.null(getLoadedDLLs()$libkhiva_c[[3]])){
      dyn.unload('/usr/local/lib/libaf.so.3')
      dyn.unload(shared.library)
    }
    dyn.load('/usr/local/lib/libaf.so.3', local = FALSE)
    dyn.load(shared.library, local = FALSE)
  }
  library("bit64")
}

package <- PackageName()

.onLoad <- function(lib.name, pkg.name) {
  LoadLibraries()
}

#' @export
KHIVABackend <- function() {
  list(
    KHIVA_BACKEND_DEFAULT = 0,
    KHIVA_BACKEND_CPU = 1,
    KHIVA_BACKEND_CUDA = 2,
    KHIVA_BACKEND_OPENCL = 4
  )
}

#' @export
KHIVABackendFromOrdinal <- function(number) {
  switch(
    toString(number),
    "0" = {
      return(KHIVABackend()[1])
    },
    "1" = {
      return(KHIVABackend()[2])
    },
    "2" = {
      return(KHIVABackend()[3])
    },
    "4" = {
      return(KHIVABackend()[4])
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
                PACKAGE = package))
}

#' SetBackend
#'
#' Set the backend.
#' @param backend The desired backend.
#' @export
SetBackend <- function(backend) {
  try(out <- .C("set_backend",
                as.integer(backend),
                PACKAGE = package))
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
                PACKAGE = package))

  return(KHIVABackendFromOrdinal(out$result))
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
                PACKAGE = package))

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
                PACKAGE = package))
}

#' GetDeviceID
#'
#' Get the device id.
#'
#' @return The active device.
#' @export
GetDeviceID <- function() {
  try(out <- .C("get_device_id",
                result = as.integer(seq(
                  length = 1,
                  from = 0,
                  to = 0
                )),
                PACKAGE = package))
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
                PACKAGE = package))
  newList <- list("result" = out$result)

  return(newList)
}

#' Version
#'
#' Returns a string with the current version of the library.
#'
#' @return A string with the current version of the library.
#' @export
Version <- function() {
  try(out <- .C("version",
                result = paste(rep(" ", 40), collapse = ""),
                PACKAGE = package))
  newList <- list("result" = out$result)

  return(newList)
}
