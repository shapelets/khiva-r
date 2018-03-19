#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

#' @brief Calculates de sum over the square values of the time series.
#' 
#' @param  time.series List of time series double arrays.
#' @return List with the Absolute Energy. 
#' @export 

AbsEnergy <- function(time.series) {

  time.series.length <- as.integer64(length(time.series[[1]]))
  concatenated.time.series <- as.double(apply( cbind( time.series ), 1, unlist))
  number.of.time.series <- as.integer64(length(time.series))
  
  try(
    out <- .C("abs_energy",
              concatenated.time.series,
              time.series.length,
              number.of.time.series,
              result = as.double(seq(length = number.of.time.series), from = 0, to = 0),
              PACKAGE='libTSALIB.dylib')
  )
  
  newList <- list("result" = out$result)
  
  return(newList)
}

#' @brief Calculates the sum over the absolute value of consecutive 
#' changes in the time series
#' 
#' @param  time.series List of time.series double arrays.
#' @return List with the absoluteSumOfChanges
#' @export 

AbsoluteSumOfChanges <- function(time.series) {

  time.series.length <- as.integer64(length(time.series[[1]]))
  concatenated.time.series <- as.double(apply( cbind( time.series ), 1 ,unlist))
  number.of.time.series <- as.integer64(length(time.series))
  
  try(
    out <- .C("absolute_sum_of_changes",
              concatenated.time.series,
              time.series.length,
              number.of.time.series,
              result = as.double(seq(length = number.of.time.series), from = 0, to = 0),
              PACKAGE='libTSALIB.dylib')
  )
  
  newList <- list("result" = out$result)
  
  return(newList)
}

#' @brief c3 function.
#' 
#' @param time.series List of time series double arrays.
#' @param lag The lag.
#' @return The non-linearity value for the given time series.
#' @export 

C3 <- function(tss, lag) {

  tss.length <- as.integer64(length(tss[[1]]))
  tss.concatenated <- as.double(apply(cbind(tss), 1, unlist))
  tss.number.of.ts <- as.integer64(length(tss))
  
  try(
    out <- .C("c3",
              tss.concatenated,
              tss.length,
              tss.number.of.ts,
              as.integer64(lag),
              result = as.double(seq(length = tss.number.of.ts), from = 0, to = 0),
              PACKAGE='libTSALIB.dylib')
  )
  
  r.result <- list("result" = out$result)
  
  return(r.result)
}

#' @brief cidCe function.
#' 
#' @param tss List of time series double arrays.
#' @param z.normalize Controls whether the time series should be z-normalized or not.
#' @return The complexity value for the given time series.
#' @export 

CidCe <- function(tss, z.normalize) {

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