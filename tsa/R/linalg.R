#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

#' Lls
#' 
#' Calculates the minimum norm least squares solution \eqn{x} \eqn{(||A·x − b||^2)} to \eqn{A·x = b}. This
# 'function uses the singular value decomposition function of Arrayfire. The actual formula that this function computes
#' is \eqn{x = V·D\dagger·U^T·b}. Where \eqn{U} and \eqn{V} are orthogonal matrices and \eqn{Ddagger} contains
#' the inverse values of the singular values contained in \eqn{D} if they are not zero, and zero otherwise.
#' 
#' @param a Coefficients of the linear equation problem to solve.
#' @param b The measured values.
#' @return Contains the solution to the linear equation problem minimizing the norm 2.
#' @export
Lls <- function(a, b) {
  a.l <- as.integer64(length(a[[1]]))
  a.n <- as.integer64(length(a))
  a.concatenated <- as.double(apply(cbind(a), 1, unlist))
  
  b.l <- as.integer64(length(b))
  
  try(out <-
        .C(
          "lls",
          a.concatenated,
          a.l,
          a.n,
          b,
          b.l,
          result = as.double(seq(
            length = (a.n),
            from = 0,
            to = 0
          )),
          PACKAGE = library
        ))
  return(out$result)
}