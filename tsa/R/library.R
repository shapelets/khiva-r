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

#' @brief Get the device info.
#'
#' @export
Info <- function() {
  try(out <- .C("info",
                PACKAGE = library))
}

#' @brief Set the backend.
#'
#' @export
SetBackend <- function(backend) {
  try(out <- .C("set_backend",
                as.integer(backend),
                PACKAGE = library))
}

#' @brief Get the active backend.
#'
#' @result The active backend.
#' @export
GetBackend <- function() {
  try(out <- .C("get_backend",
                result = as.integer(seq(
                  length = 1,
                  from = 0,
                  to = 0
                )),
                PACKAGE = library))
  
  newList <- list("result" = out$result)
  
  return(newList)
}

#' @brief Get the active available backends.
#'
#' @result The available backends.
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

#' @brief Set the device.
#'
#' @param device The desired device.
#' @export
SetDevice <- function(device) {
  try(out <- .C("set_device",
                as.integer(device),
                PACKAGE = library))
}

#' @brief Get the device
#'
#' @param device The active device.
#' @export
GetDevice <- function() {
  try(out <- .C("get_device",
                result = as.integer(seq(
                  length = 1,
                  from = 0,
                  to = 0
                )),
                PACKAGE = library))
  newList <- list("result" = out$result)
  
  return(newList)
}