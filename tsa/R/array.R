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
  ty = -1
  if (type == "f32") {
    ty = 0
  }
  if (type == "c32") {
    stop("unsupported data type")
  }
  if (type == "f64") {
    ty = 2
  }
  if (type == "c64") {
    ty = 3
  }
  if (type == "b8") {
    ty = 4
  }
  if (type == "s32") {
    ty = 5
  }
  if (type == "u32") {
    ty = 6
  }
  if (type == "u8") {
    stop("unsupported data type")
  }
  if (type == "s64") {
    ty = 8
  }
  if (type == "u64") {
    ty = 9
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

#' GetRType
#'
#' Gets the R type corresponding to the tsa Array type of a datafra.me.
#'
#' @param  a R data.frame
#' @return Type of the elements in the data.frame (All should have the same type).
#' @export
GetRType <- function(a) {
  atype <- class(a[[1]])
  if (atype == "numeric" && !is.null(attributes(a[[1]])$Csingle)) {
    atype <- "f32"
  } else if (atype == "numeric" &&
             is.null(attributes(a[[1]])$Csingle)) {
    atype <- "f64"
  } else if (atype == "complex") {
    atype <- "c64"
  } else if (atype == "logical") {
    atype <- "b8"
  } else if (atype == "integer") {
    atype <- "s32"
  } else if (atype == "integer64") {
    atype <- "s64"
  }
  return(atype)
}

#' Array
#'
#' Creates a tsa Array from a data.frame.
#' Every columns of the data.frame represents an array and every one should have the same type and same dimensions
#'
#' @param  a data.frame which columns have same types and same dimensions.
#' @return A TSA array created from the data stored in the data.frame.
#' @export
Array <- function(a) {
  d <- as.integer64(dim(a))
  ty = getTypeID(GetRType(a))
  a <- c(apply(cbind(a), 2, unlist))
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
#' @param a data.frame which columns have same types and same dimensions.
#' @export
display <- function(a) {
  try(out <- .C("print", ptr = a@ptr, PACKAGE = package))
  eval.parent(substitute(a@ptr <- out$ptr))
}

#' getDims
#'
#' Gets the TSA array dimensions.
#'
#' @param a data.frame which columns have same types and same dimensions.
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
#' @return The data stored by the TSA array in a vector of its corresponding R DATA type.
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
              result = as.single(seq(length = elements)),
              PACKAGE = package
            ))
    },
    "1" = {
      stop("unsupported data type")
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
      try(out <-
            .C(
              "get_data",
              ptr = b@ptr,
              result = as.logical(seq(
                length = elements,
                from = 0,
                to = 0
              )),
              PACKAGE = package
            ))
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
  return(out$result)
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