#
#Copyright (c) 2018 Grumpy Cat Software S.L.
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

setClass("Array", representation(ptr = "integer64"))

#' createArray
#'
#' Creates an Array from a pointer.
#'
#' @param p Pointer to Array.
#' @return Array object.
#' @export
createArray <- function(p) {
  result <- new("Array", ptr = p)
  return (result)
}

#' getTypeID
#'
#' Gets the type ID.
#'
#' @param  type TSA array type.
#' @return Type id.
#' @export
getTypeID <- function(type) {
  ty <- -1
  if (type == "f32") {
    ty <- 0
  }
  if (type == "c32") {
    ty <- 1
  }
  if (type == "f64") {
    ty <- 2
  }
  if (type == "c64") {
    ty <- 3
  }
  if (type == "b8") {
    ty <- 4
  }
  if (type == "s32") {
    ty <- 5
  }
  if (type == "u32") {
    ty <- 6
  }
  if (type == "u8") {
    stop("unsupported data type")
  }
  if (type == "s64") {
    ty <- 8
  }
  if (type == "u64") {
    ty <- 9
  }
  if (type == "s16") {
    stop("unsupported data type")
  }
  if (type == "u16") {
    stop("unsupported data type")
  }
  if (ty == -1) {
    stop("unsupported data type")
  }
  ty = as.integer(ty)
  return (ty)
}

#' getTypeName
#'
#' Gets the type Name.
#'
#' @param  type TSA array id.
#' @return Type Name.
#' @export
getTypeName <- function(type) {
  ty <- "-1"
  if (type == 0) {
    ty <- "f32"
  }
  if (type == 1) {
    stop("unsupported data type")
  }
  if (type == 2) {
    ty <- "f64"
  }
  if (type == 3) {
    ty <- "c64"
  }
  if (type == 4) {
    ty <- "b8"
  }
  if (type == 5) {
    ty <- "s32"
  }
  if (type == 6) {
    ty = "u32"
  }
  if (type == 7) {
    stop("unsupported data type")
  }
  if (type == 8) {
    ty <- "s64"
  }
  if (type == 9) {
    ty <- "u64"
  }
  if (type == 10) {
    stop("unsupported data type")
  }
  if (type == 11) {
    stop("unsupported data type")
  }
  if (ty == "-1") {
    stop("unsupported data type")
  }
  return (ty)
}

#' getBoolean
#'
#' Gets the logical R value from C++ Boolean data type.
#' @param a Array of bits
#' @return Logical R array with the  R logical equivalent of a.
getBoolean <- function(a) {
  l <- length(a)
  iter <- (l / 8 - 1)
  result <- logical(length = iter + 1)
  for (i in 0:iter) {
    result[i + 1] <- a[i * 8 + 1]
  }
  return(result)
}

#' Array
#'
#' Creates a tsa Array from a R array.
#'
#'
#' @param  a array which columns have same types and same dimensions.
#' @return A TSA array created from the data stored in the array.
#' @export
Array <- function(a, type = "f32") {
  d <- as.integer64(dim(a))
  ty = getTypeID(type)
  a <- c(a)
  if (ty == 0) {
    try(out <- .C(
      "create_array",
      as.single(a),
      as.integer(length(d)),
      d,
      result = as.integer64(0),
      as.integer(ty),
      PACKAGE = package
    ))
  } else if (ty == 1) {
    try(out <- .C(
      "create_array",
      as.single(c(rbind(Re(
        a
      ), Im(
        a
      )))),
      as.integer(length(d)),
      d,
      result = as.integer64(0),
      as.integer(1),
      PACKAGE = package
    ))
  } else {
    try(out <- .C(
      "create_array",
      a,
      as.integer(length(d)),
      d,
      result = as.integer64(0),
      as.integer(ty),
      PACKAGE = package
    ))
  }
  
  result <- createArray(out$result)
  return (result)
}

#' getType
#'
#' Gets the tsa Array type ID.
#'
#' @param  arr TSA Array.
#' @return The type ID of arr.
#' @export
getType <- function(arr) {
  try(out <- .C(
    "get_type",
    ptr = arr@ptr,
    result = as.integer(0),
    PACKAGE = package
  ))
  eval.parent(substitute(arr@ptr <- out$ptr))
  return(out$result)
}

#' Display
#'
#' Displays the TSA array.
#'
#' @param a TSA array.
#' @export
display <- function(a) {
  try(out <- .C("print", ptr = a@ptr, PACKAGE = package))
  eval.parent(substitute(a@ptr <- out$ptr))
}

#' getDims
#'
#' Gets the TSA array dimensions.
#'
#' @param a TSA array.
#' @return TSA Array dimensions.
#' @export
getDims <- function(a) {
  try(out <-
        .C(
          "get_dims",
          ptr = a@ptr,
          result = as.integer64(seq(length = 4)),
          PACKAGE = package
        ))
  eval.parent(substitute(a@ptr <- out$ptr))
  return (out$result)
}

#' getData
#'
#' Gets the data to the host.
#'
#' @param  a TSA array of interest.
#' @return The data stored by the TSA array into a R array.
#' @export
getData <- function(a) {
  b <- a
  dims <- getDims(b)
  elements <- as.integer64(1)
  for (i in 1:4) {
    elements = elements * dims[i]
  }
  type <- getType(b)
  switch(
    toString(type),
    "0" = {
      try(out <-
            .C(
              "get_data",
              ptr = b@ptr,
              result = as.single(seq(
                length = elements,
                from = 0,
                to = 0
              )),
              PACKAGE = package
            ))
    },
    "1" = {
      try(out <-
            .C(
              "get_data",
              ptr = b@ptr,
              result = as.single(seq(
                length = 2 * elements,
                from = 0,
                to = 0
              )),
              PACKAGE = package
            ))
      r.and.i <- split(out$result, 1:2)
      out$result <-
        complex(real = as.numeric(as.character(unlist(r.and.i[[1]]))),
                imaginary = as.numeric(as.character(unlist(r.and.i[[2]]))))
    },
    "2" = {
      try(out <-
            .C(
              "get_data",
              ptr = b@ptr,
              result = as.double(seq(
                length = elements,
                from = 0,
                to = 0
              )),
              PACKAGE = package
            ))
    },
    "3" = {
      try(out <-
            .C(
              "get_data",
              ptr = b@ptr,
              result = as.complex(seq(
                length = elements,
                from = 0,
                to = 0
              )),
              PACKAGE = package
            ))
    },
    "4" = {
      l <- 8 * elements
      try(out <-
            .C(
              "get_data",
              ptr = b@ptr,
              result =  bit(length = l),
              PACKAGE = package
            ))
      out$result <- getBoolean(out$result)
    },
    "5" = {
      try(out <-
            .C(
              "get_data",
              ptr = b@ptr,
              result = as.integer(seq(
                length = elements,
                from = 0,
                to = 0
              )),
              PACKAGE = package
            ))
    },
    "6" = {
      try(out <-
            .C(
              "get_data",
              ptr = b@ptr,
              result = as.integer(seq(
                length = elements,
                from = 0,
                to = 0
              )),
              PACKAGE = package
            ))
    },
    "7" = {
      stop("unsupported data type")
    },
    "8" = {
      try(out <-
            .C(
              "get_data",
              ptr = b@ptr,
              result = as.integer64(seq(
                length = elements,
                from = 0,
                to = 0
              )),
              PACKAGE = package
            ))
    },
    "9" = {
      try(out <-
            .C(
              "get_data",
              ptr = b@ptr,
              result = as.integer64(seq(
                length = elements,
                from = 0,
                to = 0
              )),
              PACKAGE = package
            ))
    },
    "10" = {
      stop("unsupported data type")
    },
    "11" = {
      stop("unsupported data type")
    }
  )
  eval.parent(substitute(a@ptr <- out$ptr))
  return(array(out$result, dim = as.double(dims)))
}

#' deleteArray
#'
#' Releases the array.
#'
#' @param  arr TSA array to release.
#' @export
deleteArray <- function(arr) {
  try(out <- .C("delete_array", arr@ptr, PACKAGE = package))
  eval.parent(remove(arr))
}