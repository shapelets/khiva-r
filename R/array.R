#
#Copyright (c) 2019 Shapelets.io
#
#This Source Code Form is subject to the terms of the Mozilla Public
#License, v. 2.0. If a copy of the MPL was not distributed with this
#file, You can obtain one at http://mozilla.org/MPL/2.0/.

#'KHIVA Array class
#'
#' @export
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
#' @param  type KHIVA array type.
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
#' @param  type KHIVA array id.
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
#' Creates a khiva Array from a R array.
#'
#' @param  a array which columns have same types and same dimensions.
#' @param type Desired khiva Array type. By default: f32
#' @return A KHIVA array created from the data stored in the array.
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
  } else if (ty == 4) {
    try(out <- .C(
      "create_array",
      as.raw(a),
      as.integer(length(d)),
      d,
      result = as.integer64(0),
      as.integer(ty),
      PACKAGE = package
    ))
  }
  else {
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
#' Gets the khiva Array type ID.
#'
#' @param  arr KHIVA Array.
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
#' Displays the KHIVA array.
#'
#' @param a KHIVA array.
#' @export
display <- function(a) {
  try(out <- .C("display", ptr = a@ptr, PACKAGE = package))
  eval.parent(substitute(a@ptr <- out$ptr))
}

#' getDims
#'
#' Gets the KHIVA array dimensions.
#'
#' @param a KHIVA array.
#' @return KHIVA Array dimensions.
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
#' @param  a KHIVA array of interest.
#' @return The data stored by the KHIVA array into a R array.
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
#' @param  arr KHIVA array to release.
deleteArray <- function(arr) {
  try(out <- .C("delete_array", arr@ptr, PACKAGE = package))
  eval.parent(remove(arr))
}

#' Adds two arrays.
#' @param e1 Khiva array.
#' @param e2 Khiva array.
setMethod("+", signature(e1 = "Array", e2 = "Array"), function (e1, e2) {
  try(out <-
        .C(
          "khiva_add",
          ptr.e1 = e1@ptr,
          ptr.e2 = e2@ptr,
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(e1@ptr <- out$ptr.e1))
  eval.parent(substitute(e2@ptr <- out$ptr.e2))
  return(createArray(out$result))
})

#' Subtracts two arrays.
#' @param e1 Khiva array.
#' @param e2 Khiva array.
setMethod("-", signature(e1 = "Array", e2 = "Array"), function (e1, e2) {
  try(out <-
        .C(
          "khiva_sub",
          ptr.e1 = e1@ptr,
          ptr.e2 = e2@ptr,
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(e1@ptr <- out$ptr.e1))
  eval.parent(substitute(e2@ptr <- out$ptr.e2))
  return(createArray(out$result))
})

#' Multiplies two arrays.
#' @param e1 Khiva array.
#' @param e2 Khiva array.
setMethod("*", signature(e1 = "Array", e2 = "Array"), function (e1, e2) {
  try(out <-
        .C(
          "khiva_mul",
          ptr.e1 = e1@ptr,
          ptr.e2 = e2@ptr,
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(e1@ptr <- out$ptr.e1))
  eval.parent(substitute(e2@ptr <- out$ptr.e2))
  return(createArray(out$result))
})

#' Divides two arrays.
#' @param e1 Khiva array.
#' @param e2 Khiva array.
setMethod("/", signature(e1 = "Array", e2 = "Array"), function (e1, e2) {
  try(out <-
        .C(
          "khiva_div",
          ptr.e1 = e1@ptr,
          ptr.e2 = e2@ptr,
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(e1@ptr <- out$ptr.e1))
  eval.parent(substitute(e2@ptr <- out$ptr.e2))
  return(createArray(out$result))
})

#' Performs the modulo operation.
#' @param e1 Khiva array.
#' @param e2 Khiva array.
setMethod("%%", signature(e1 = "Array", e2 = "Array"), function (e1, e2) {
  try(out <-
        .C(
          "khiva_mod",
          ptr.e1 = e1@ptr,
          ptr.e2 = e2@ptr,
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(e1@ptr <- out$ptr.e1))
  eval.parent(substitute(e2@ptr <- out$ptr.e2))
  return(createArray(out$result))
})

#' Powers an array by other one (Element-wise).
#' @param e1 Khiva array.
#' @param e2 Khiva array.
setMethod("^", signature(e1 = "Array", e2 = "Array"), function (e1, e2) {
  try(out <-
        .C(
          "khiva_pow",
          ptr.e1 = e1@ptr,
          ptr.e2 = e2@ptr,
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(e1@ptr <- out$ptr.e1))
  eval.parent(substitute(e2@ptr <- out$ptr.e2))
  return(createArray(out$result))
})

#' Compares (element-wise) if an Array is less than other.
#' @param e1 Khiva array.
#' @param e2 Khiva array.
setMethod("<", signature(e1 = "Array", e2 = "Array"), function (e1, e2) {
  try(out <-
        .C(
          "khiva_lt",
          ptr.e1 = e1@ptr,
          ptr.e2 = e2@ptr,
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(e1@ptr <- out$ptr.e1))
  eval.parent(substitute(e2@ptr <- out$ptr.e2))
  return(createArray(out$result))
})

#' Compares (element-wise) if an Array is greater than other.
#' @param e1 Khiva array.
#' @param e2 Khiva array.
setMethod(">", signature(e1 = "Array", e2 = "Array"), function (e1, e2) {
  try(out <-
        .C(
          "khiva_gt",
          ptr.e1 = e1@ptr,
          ptr.e2 = e2@ptr,
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(e1@ptr <- out$ptr.e1))
  eval.parent(substitute(e2@ptr <- out$ptr.e2))
  return(createArray(out$result))
})

#' Compares (element-wise) if an Array is less or equal than other.
#' @param e1 Khiva array.
#' @param e2 Khiva array.
setMethod("<=", signature(e1 = "Array", e2 = "Array"), function (e1, e2) {
  try(out <-
        .C(
          "khiva_le",
          ptr.e1 = e1@ptr,
          ptr.e2 = e2@ptr,
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(e1@ptr <- out$ptr.e1))
  eval.parent(substitute(e2@ptr <- out$ptr.e2))
  return(createArray(out$result))
})

#' Compares (element-wise) if an Array is greater or equal than other.
#' @param e1 Khiva array.
#' @param e2 Khiva array.
setMethod(">=", signature(e1 = "Array", e2 = "Array"), function (e1, e2) {
  try(out <-
        .C(
          "khiva_ge",
          ptr.e1 = e1@ptr,
          ptr.e2 = e2@ptr,
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(e1@ptr <- out$ptr.e1))
  eval.parent(substitute(e2@ptr <- out$ptr.e2))
  return(createArray(out$result))
})

#' Compares (element-wise) if an Array is equal to other.
#' @param e1 Khiva array.
#' @param e2 Khiva array.
setMethod("==", signature(e1 = "Array", e2 = "Array"), function (e1, e2) {
  try(out <-
        .C(
          "khiva_eq",
          ptr.e1 = e1@ptr,
          ptr.e2 = e2@ptr,
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(e1@ptr <- out$ptr.e1))
  eval.parent(substitute(e2@ptr <- out$ptr.e2))
  return(createArray(out$result))
})

#' Compares (element-wise) if an Array is not equal than other.
#' @param e1 Khiva array.
#' @param e2 Khiva array.
setMethod("!=", signature(e1 = "Array", e2 = "Array"), function (e1, e2) {
  try(out <-
        .C(
          "khiva_ne",
          ptr.e1 = e1@ptr,
          ptr.e2 = e2@ptr,
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(e1@ptr <- out$ptr.e1))
  eval.parent(substitute(e2@ptr <- out$ptr.e2))
  return(createArray(out$result))
})

#' Performs an AND operation (element-wise) with two arrays.
#' @param e1 Khiva array.
#' @param e2 Khiva array.
setMethod("&", signature(e1 = "Array", e2 = "Array"), function (e1, e2) {
  try(out <-
        .C(
          "khiva_bitand",
          ptr.e1 = e1@ptr,
          ptr.e2 = e2@ptr,
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(e1@ptr <- out$ptr.e1))
  eval.parent(substitute(e2@ptr <- out$ptr.e2))
  return(createArray(out$result))
})

#' Performs an OR operation (element-wise) with two Arrays.
#' @param e1 Khiva array.
#' @param e2 Khiva array.
setMethod("|", signature(e1 = "Array", e2 = "Array"), function (e1, e2) {
  try(out <-
        .C(
          "khiva_bitor",
          ptr.e1 = e1@ptr,
          ptr.e2 = e2@ptr,
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(e1@ptr <- out$ptr.e1))
  eval.parent(substitute(e2@ptr <- out$ptr.e2))
  return(createArray(out$result))
})

#' matMult
#'
#' Performs a matrix multiplication.
#'
#' @param e1 Khiva array.
#' @param e2 Khiva array.
setGeneric("matMult", function(e1, e2) {
  standardGeneric("matMult")
})

#' matMult
#'
#' Performs a matrix multiplication.
#'
#' @param e1 Khiva array.
#' @param e2 Khiva array.
#' @export
setMethod("matMult", signature(e1 = "Array", e2 = "Array"), function(e1, e2) {
  try(out <-
        .C(
          "khiva_matmul",
          ptr.e1 = e1@ptr,
          ptr.e2 = e2@ptr,
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(e1@ptr <- out$ptr.e1))
  eval.parent(substitute(e2@ptr <- out$ptr.e2))
  return(createArray(out$result))
})

#' not.khiva
#'
#' Returns the complement of the input boolean KHIVA array.
#'
#' @param e1 Khiva array.
setGeneric("not.khiva", function(e1) {
  standardGeneric("not.khiva")
})

#' not.khiva
#'
#' Returns the complement of the input boolean KHIVA array.
#'
#' @param e1 Khiva array.
#' @export
setMethod("not.khiva", signature(e1 = "Array"), function(e1) {
  try(out <-
        .C(
          "khiva_not",
          ptr.e1 = e1@ptr,
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(e1@ptr <- out$ptr.e1))
  return(createArray(out$result))
})

#' xor.khiva
#'
#' Returns a exclusive-or between each pair of elements from two KHIVA arrays (element-wise).
#' 
#' @param e1 Khiva array.
#' @param e2 Khiva array.
setGeneric("xor.khiva", function(e1, e2) {
  standardGeneric("xor.khiva")
})

#' xor.khiva
#'
#' Returns a exclusive-or between each pair of elements from two KHIVA arrays (element-wise).
#' 
#' @param e1 Khiva array.
#' @param e2 Khiva array.
#' @export
setMethod("xor.khiva", signature(e1 = "Array", e2 = "Array"), function(e1, e2) {
  try(out <-
        .C(
          "khiva_bitxor",
          ptr.e1 = e1@ptr,
          ptr.e2 = e2@ptr,
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(e1@ptr <- out$ptr.e1))
  eval.parent(substitute(e2@ptr <- out$ptr.e2))
  return(createArray(out$result))
})

#' bitShiftL
#'
#' Shifts each element of the input KHIVA array n positions to left.
#'
#' @param e1 Khiva array.
#' @param n Bits to shift.
setGeneric("bitShiftL", function(e1, n) {
  standardGeneric("bitShiftL")
})

#' bitShiftL
#'
#' Shifts each element of the input KHIVA array n positions to left.
#'
#' @param e1 Khiva array.
#' @param n Bits to shift.
#' @export
setMethod("bitShiftL", signature(e1 = "Array", n = "numeric"), function(e1, n) {
  try(out <-
        .C(
          "khiva_bitshiftl",
          ptr.e1 = e1@ptr,
          as.integer(n),
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(e1@ptr <- out$ptr.e1))
  return(createArray(out$result))
})

#' bitShifR
#'
#' Shifts each element of the input KHIVA array n positions to right.
#'
#' @param e1 Khiva array.
#' @param n bits to shift.
setGeneric("bitShiftR", function(e1, n) {
  standardGeneric("bitShiftR")
})

#' bitShifR
#'
#' Shifts each element of the input KHIVA array n positions to right.
#'
#' @param e1 Khiva array.
#' @param n bits to shift.
#' @export
setMethod("bitShiftR", signature(e1 = "Array", n = "numeric"), function(e1, n) {
  try(out <-
        .C(
          "khiva_bitshiftr",
          ptr.e1 = e1@ptr,
          as.integer(n),
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(e1@ptr <- out$ptr.e1))
  return(createArray(out$result))
})

#' copy
#'
#' Performs a deep copy of the array.
#'
#' @param a Khiva array.
setGeneric("copy", function(a) {
  standardGeneric("copy")
})

#' copy
#'
#' Performs a deep copy of the array.
#'
#' @param a Khiva array.
#' @export
setMethod("copy", signature(a = "Array"), function(a) {
  try(out <-
        .C(
          "copy",
          ptr.a = a@ptr,
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(a@ptr <- out$ptr.a))
  return(createArray(out$result))
})

#' asType
#'
#' Returns a copy of the KHIVA array with the specified base type.
#' 
#' @param a Khiva array.
#' @param type Desired type.
setGeneric("asType", function(a, type) {
  standardGeneric("asType")
})

#' asType
#'
#' Returns a copy of the KHIVA array with the specified base type.
#' 
#' @param a Khiva array.
#' @param type Desired type.
#' @export
setMethod("asType", signature(a = "Array", type = "character"), function(a, type) {
  ty <- getTypeID(type)
  try(out <-
        .C(
          "khiva_as",
          ptr.a = a@ptr,
          as.integer(ty),
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(a@ptr <- out$ptr.a))
  return(createArray(out$result))
})

#' getRow
#'
#' Returns the row specified by index.
#'
#' @param a Khiva array.
#' @param index Row index.
setGeneric("getRow", function(a, index) {
  standardGeneric("getRow")
})

#' getRow
#'
#' Returns the row specified by index.
#'
#' @param a Khiva array.
#' @param index Row index.
#' @export
setMethod("getRow", signature(a = "Array", index = "numeric"), function(a, index) {
  try(out <-
        .C(
          "khiva_row",
          ptr.a = a@ptr,
          as.integer(index),
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(a@ptr <- out$ptr.a))
  return(createArray(out$result))
})

#' getCol
#'
#' Returns the column specified by index.
#'
#' @param a Khiva array.
#' @param index Column index.
setGeneric("getCol", function(a, index) {
  standardGeneric("getCol")
})

#' getCol
#'
#' Returns the column specified by index.
#'
#' @param a Khiva array.
#' @param index Column index.
#' @export
setMethod("getCol", signature(a = "Array", index = "numeric"), function(a, index) {
  try(out <-
        .C(
          "khiva_col",
          ptr.a = a@ptr,
          as.integer(index),
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(a@ptr <- out$ptr.a))
  return(createArray(out$result))
})

#' getRows
#'
#' Returns a sequence of rows limited by first and last, both included.
#'
#' @param a Khiva Array.
#' @param first Index of the first row.
#' @param last Index of the last row.
setGeneric("getRows", function(a, first, last) {
  standardGeneric("getRows")
})

#' getRows
#'
#' Returns a sequence of rows limited by first and last, both included.
#'
#' @param a Khiva Array.
#' @param first Index of the first row.
#' @param last Index of the last row.
#' @export
setMethod("getRows", signature(a = "Array", first = "numeric", last = "numeric"), function(a, first, last) {
  try(out <-
        .C(
          "khiva_rows",
          ptr.a = a@ptr,
          as.integer(first),
          as.integer(last),
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(a@ptr <- out$ptr.a))
  return(createArray(out$result))
})

#' getCols
#'
#' Returns a sequence of columns limited by first and last, both included.
#'
#' @param a Khiva Array.
#' @param first Index of the first column.
#' @param last Index of the second column.
setGeneric("getCols", function(a, first, last) {
  standardGeneric("getCols")
})

#' getCols
#'
#' Returns a sequence of columns limited by first and last, both included.
#'
#' @param a Khiva Array.
#' @param first Index of the first column.
#' @param last Index of the second column.
#' @export
setMethod("getCols", signature(a = "Array", first = "numeric", last = "numeric"), function(a, first, last) {
  try(out <-
        .C(
          "khiva_cols",
          ptr.a = a@ptr,
          as.integer(first),
          as.integer(last),
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(a@ptr <- out$ptr.a))
  return(createArray(out$result))
})

#' transpose
#'
#' Transpose the KHIVA Array.
#'
#' @param a Khiva Array.
#' @param conjugate Indicates if the tranpose is conjugated or not.
setGeneric("transpose", function(a, conjugate) {
  standardGeneric("transpose")
})

#' transpose
#'
#' Transpose the KHIVA Array.
#'
#' @param a Khiva Array.
#' @param conjugate Indicates if the tranpose is conjugated or not.
#' @export
setMethod("transpose", signature(a = "Array", conjugate = "logical"), function(a, conjugate) {
  try(out <-
        .C(
          "khiva_transpose",
          ptr.a = a@ptr,
          as.logical(conjugate),
          result = as.integer64(0),
          PACKAGE = package
        ))
  eval.parent(substitute(a@ptr <- out$ptr.a))
  return(createArray(out$result))
})
