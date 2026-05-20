################################################################################

#' Easy messages
#'
#' @param ... Arguments passed on to [base::sprintf].
#'
#' @export
#'
#' @seealso [base::sprintf]
#'
#' @examples
#' printf("My name is %s.", "Florian")
printf <- function(...) cat(sprintf(...))

################################################################################

#' @rdname printf
#' @export
message2 <- function(...) message(sprintf(...))

################################################################################

#' @rdname printf
#' @param depth Depth of the function to capture the name from.
#'   Use `NULL` (default) to omit the function name, `-1` for the direct caller,
#'   `-2` for the caller's caller (e.g. inside `assert_*` wrappers).
#' @export
warning2 <- function(..., depth = NULL) {
  fun <- if (!is.null(depth)) {
    call <- sys.call(depth)
    if (!is.null(call)) paste0("[In ", deparse(call[[1]]), "()] ") else ""
  } else ""
  warning(paste0(fun, sprintf(...)), call. = FALSE)
}

################################################################################

#' @rdname printf
#' @export
stop2 <- function(..., depth = NULL) {
  fun <- if (!is.null(depth)) {
    call <- sys.call(depth)
    if (!is.null(call)) paste0("[In ", deparse(call[[1]]), "()] ") else ""
  } else ""
  stop(paste0(fun, sprintf(...)), call. = FALSE)
}

################################################################################
