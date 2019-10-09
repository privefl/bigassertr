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
#' @export
warning2 <- function(...) warning(sprintf(...), call. = FALSE)

################################################################################

#' @rdname printf
#' @export
stop2 <- function(...) stop(sprintf(...), call. = FALSE)

################################################################################
