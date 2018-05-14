#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

#' Lls
#'
#' Calculates the minimum norm least squares solution \eqn{x} \eqn{(||A·x - b||^2)} to \eqn{A·x = b}. This
# 'function uses the singular value decomposition function of Arrayfire. The actual formula that this function computes
#' is \eqn{x = V·D\dagger·U^T·b}. Where \eqn{U} and \eqn{V} are orthogonal matrices and \eqn{Ddagger} contains
#' the inverse values of the singular values contained in \eqn{D} if they are not zero, and zero otherwise.
#'
#' @param a TSA Array with the coefficients of the linear equation problem to solve.
#' @param b TSA Array with the measured values.
#' @return Contains the solution to the linear equation problem minimizing the norm 2.
#' @export
Lls <- function(arr.a, arr.b) {
  try(out <-
        .C(
          "lls",
          a.ptr = arr.a@ptr,
          b.ptr = arr.b@ptr,
          b = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(arr.a@ptr <- out$a.ptr))
  eval.parent(substitute(arr.b@ptr <- out$b.ptr))
  return(createArray(out$b))
}