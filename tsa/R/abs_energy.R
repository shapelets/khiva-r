#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.
#
#
#' @brief Calculates de sum over the square values of the time series.
#' 
#' @param  time.series List of time series double arrays.
#' 
#' @return List with the Absolute Energy. 
#' @export 

AbsEnergy <- function(time.series){
  
  shared.library <-system.file("extdata","libTSALIB.dylib",package="tsa")
  dyn.load(shared.library,PACKAGE='libTSALIB.dylib')
  
  library("bit64")
  
  time.series.length <- as.integer64(length(time.series[[1]]))
  concatenated.time.series <- as.double(apply( cbind( time.series ), 1 ,unlist))
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