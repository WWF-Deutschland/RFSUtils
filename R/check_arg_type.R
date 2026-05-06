#' Test argument type
#'
#' @description A utility function to check if arguments are of the expected type(s) and return meaningful error messages.
#'
#' @param arg The argument to check. Can be a single value or a named list of values to check.
#' @param expected_types A character vector of expected types (e.g., c("numeric", "integer")).
#' When more than two types are provided, arg may be EITHER type but doesn't have to
#' be both. If a named list is provided for 'arg' all elements of the list are expected
#' to fulfill these expected types.
#' @param arg_name The name of the argument (for error message). Ignored if a named
#' list is provided.
#' @param msg Character. Message to be printed if type doesn't match. This string
#' will be passed to `sprintf()`. Requires placeholders for two strings that will
#' be replaced with 'arg_name' (or name from list) and 'expected_types'
#' @param allow_null Logical. If arg is not of the expected type, can it still pass
#' the check if it is NULL. Default (FALSE), arg must have a value that matches
#' expected_types. If TRUE, 'arg' may be NULL.
#'
#' @return NULL if check passes, otherwise throws an error
#' @export
#'
#' @examples
#' check_arg_type(5, "numeric", "my_arg") ## Works
#' #check_arg_type(1, "character", "my_arg") ## Fails
#' #check_arg_type(1, c("character", "factor"), "my_arg", msg = "'%s' IS WRONG. NOT %s!!")
#' #check_arg_type(list(NUM = 1, NUM2 = 6, NUM3 = "X"), c("numeric"), "my_args") ## Fails
#' ## If we just provide an unnamed list we can test for type list.
#' check_arg_type(list(1, 2, "text"), "list", "my_args")
check_arg_type <- function(arg, expected_types, arg_name,
                           msg = "'%s' must be of type %s", allow_null = FALSE) {

  # Helper function to check a single argument
  check_single_arg <- function(single_arg, expected_types, arg_name, msg) {
    is_valid <- any(sapply(expected_types, \(type) {

      ## Feels inefficient because we're reprinting single_arg
      ## leave for now
      type_check <- switch(type,
                           "numeric" = is.numeric(single_arg),
                           "integer" = is.integer(single_arg),
                           "character" = is.character(single_arg),
                           "logical" = is.logical(single_arg),
                           "data.frame" = inherits(single_arg, "data.frame"),
                           "function" = is.function(single_arg),
                           "factor" = is.factor(single_arg),
                           "RasterLayer" = inherits(single_arg, "RasterLayer"),
                           "SpatRaster" = inherits(single_arg, "SpatRaster"),
                           "sf" = inherits(single_arg, "sf"),
                           "sfc" = inherits(single_arg, "sfc"),
                           "NULL" = is.null(single_arg),
                           "list" = is.list(single_arg),
                           "function" = is.function(single_arg),
                           FALSE)

      null_check <- if (allow_null) {
        is.null(single_arg)
      } else {
        FALSE
      }

      return(any(c(type_check, null_check)))

    }))

    if (!is_valid) {
      stop(sprintf(msg,
                   arg_name,
                   paste(expected_types, collapse = " or ")))
    }
    return(TRUE)
  }

  # If arg is a list, check each element
  if (is.list(arg) && !inherits(arg, "data.frame") && !is.null(names(arg))) {

    # Check each element in the list
    for (i in seq_along(arg)) {
      check_single_arg(arg[[i]], expected_types, names(arg)[i], msg)
    }

  } else {
    # Check single argument
    check_single_arg(arg, expected_types, arg_name, msg)
  }

  return(invisible(TRUE))
}
