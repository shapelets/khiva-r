#
#Copyright (c) 2019 Shapelets.io
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

#' GroupBy
#'
#' Group by operation in the input array using n.columns.key columns as group keys and n.columns.value columns as
#' values. The data is expected to be sorted. The aggregation function determines the operation to aggregate the values.
#'
#' @param arr KHIVA Array with the time series.
#' @param aggregation.function Function to be used in the aggregation. It receives an integer which indicates the
#' function to be applied:
#' {
#'   0 : mean,
#'   1 : median
#'   2 : min,
#'   3 : max,
#'   4 : stdev,
#'   5 : var,
#'   default : mean
#' }
#' @param n.columns.key Number of columns conforming the key.
#' @param n.columns.value Number of columns conforming the value (they are expected to be consecutive to the column
#' keys).
#'
#' @return KHIVA Array with the values of the group keys aggregated using the aggregation_function.
#' @export
GroupBy <-
  function(arr,
           aggregation.function,
           n.columns.key = 1,
           n.columns.value = 1) {
    try(out <-
          .C(
            "group_by",
            ptr = arr@ptr,
            as.integer(aggregation.function),
            as.integer(n.columns.key),
            as.integer(n.columns.value),
            b = as.integer64(0),
            PACKAGE = package
          ))
    eval.parent(substitute(arr@ptr <- out$ptr))
    return(createArray(out$b))
  }
