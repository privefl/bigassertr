################################################################################

#' Assertions
#'
#' - `assert_nona()`: checks that there is no missing value.
#' - `assert_args()`: checks that `f` is a function and that it has arguments
#'   called `args.name`.
#' - `assert_lengths()`: checks that objects have the same length.
#' - `assert_one_int()`: checks that object is integer-ish of length 1.
#' - `assert_int()`: checks that values are integer-ish (or `NULL`).
#' - `assert_pos()`: checks that values are (strictly) positive.
#' - `assert_01()`: checks that values are either `0` or `1`.
#' - `assert_multiple()`: checks that there are multiple unique values.
#'   Errors if there are one unique values (or none).
#'   Warns if there are two unique values only.
#' - `assert_class()`: checks that object is of a particular class.
#' - `assert_class_or_null()`: checks that object is of a particular class
#'   (or `NULL`).
#' - `assert_all()`: checks that all values of an object are the same as `value`.
#'   Default checks that values are all `TRUE`. This function tests equality;
#'   beware precision errors (e.g. `(0.1 + 0.2) != 0.3`).
#' - `assert_dir()`: checks that directory exists, or tries to create it.
#' - `assert_exist()`: checks that file exists.
#' - `assert_noexist()`: checks that file does not exist.
#' - `assert_nodots()`: checks that `...` are not used in a function.
#' - `assert_ext()`: checks that file has a particular extension.
#' - `assert_type()`: checks that values are of a particular type.
#' - `assert_sorted()`: checks that values are sorted.
#'
#' @param x Usually a vector.
#' @param f A function.
#' @param args.name Vector of (argument) names to check.
#' @param ... Objects to check.
#' @param strict Whether to check for strict positivity? Default is `TRUE`.
#' @param class Class to check.
#' @param value Value to check.
#' @param dir.path Directory to check.
#' @param file File to check.
#' @param ext Extension to check (without the dot at the beginning).
#' @param type Type to check.
#'
#' @name assert
#'
#' @examples
#' assert_nona(1:3)
#' assert_args(assert_nona, "x")
#' assert_lengths(1:3, 4:6, as.list(1:3))
#' assert_int(c(1, 2, 3))
#' assert_01(c(0, 1, 0))
#' assert_multiple(1:3)
#' assert_class(assert_nona, "function")
#' assert_all(1:3 > 0)
#' assert_all(rep(0, 3), 0)
#' assert_dir(tempdir())
#' assert_noexist(tmp <- tempfile())
#' write("test", tmp)
#' assert_exist(tmp)
#' assert_ext("test.txt", "txt")
#' assert_type(1:3, "integer")
#' assert_sorted(1:3)
#'
#' test <- function(...) {
#'   assert_nodots()
#'   NULL
#' }
#' test()
#'
NULL

################################################################################

#' @export
#' @rdname assert
assert_nona <- function(x) {
  if (anyNA(x))
    stop2("You can't have missing values in '%s'.", deparse(substitute(x)))
}

################################################################################

#' @export
#' @rdname assert
assert_args <- function(f, args.name) {

  if (!inherits(f, "function"))
    stop2("'%s' is not a function.", deparse(substitute(f)))

  if (!all(args.name %in% names(formals(f))))
    stop2("'%s' should have argument%s named '%s'.",
          deparse(substitute(f)),
          `if`(length(args.name) > 1, "s", ""),
          toString(args.name))
}

################################################################################

#' @export
#' @rdname assert
assert_lengths <- function(...) {
  lengths <- lengths(list(...))
  if (length(lengths) > 1) {
    if (any(diff(lengths) != 0))
      stop2("Incompatibility between dimensions.")
  } else {
    stop2("You should check the lengths of at least two elements.")
  }
}

################################################################################

#' @export
#' @rdname assert
assert_int <- function(x) {
  if (!is.null(x) && !is.integer(x)) {
    if (!is.numeric(x) || any(x != trunc(x), na.rm = TRUE))
      stop2("'%s' should contain only integers.", deparse(substitute(x)))
  }
}

#' @export
#' @rdname assert
assert_one_int <- function(x) {
  var_name <- deparse(substitute(x))
  if (length(x) != 1) stop2("'%s' should be of length 1.", var_name)
  if (!is.integer(x)) {
    if (!is.numeric(x) || !isTRUE(x == trunc(x)))
      stop2("'%s' should be an integer.", var_name)
  }
}

################################################################################

#' @export
#' @rdname assert
assert_pos <- function(x, strict = TRUE) {
  var_name <- deparse(substitute(x))
  if (!is.null(x) && !is.numeric(x)) stop2("'%s' should be numeric.", var_name)
  all_pos <- isTRUE(all(`if`(strict, x > 0, x >= 0)))
  if (!all_pos)
    stop2("'%s' should have only positive values.", var_name)
}

################################################################################

#' @export
#' @rdname assert
assert_01 <- function(x)  {
  all_01 <- isTRUE(all.equal(sort(unique(x), na.last = TRUE), 0:1))
  if (!all_01)
    stop2("'%s' should be composed of 0s and 1s.", deparse(substitute(x)))
}

#' @export
#' @rdname assert
assert_multiple <- function(x) {

  nuniq <- length(unique(x))

  if (nuniq < 2) {
    stop2("'%s' should be composed of different values.", deparse(substitute(x)))
  } else if (nuniq == 2) {
    warning2("'%s' is composed of only two different levels.", deparse(substitute(x)))
  }
}

################################################################################

#' @export
#' @rdname assert
assert_class <- function(x, class)  {
  if (!inherits(x, class))
    stop2("'%s' is not of class '%s'.", deparse(substitute(x)), class)
}

#' @export
#' @rdname assert
assert_class_or_null <- function(x, class)  {
  if (!is.null(x) && !inherits(x, class))
    stop2("'%s' is not 'NULL' or of class '%s'.", deparse(substitute(x)), class)
}

################################################################################

#' @export
#' @rdname assert
assert_all <- function(x, value = TRUE) {
  if (any(x != value))
    stop2("At least one value of '%s' is different from '%s'.",
          deparse(substitute(x)), value)
}

################################################################################

#' @export
#' @rdname assert
assert_dir <- function(dir.path) {
  if (!dir.exists(dir.path)) {
    if (suppressWarnings(dir.create(dir.path))) {
      message2("Creating directory \"%s\" which didn't exist..", dir.path)
    } else {
      stop2("Problem creating directory \"%s\". Recursive path?", dir.path)
    }
  }
}

################################################################################

#' @export
#' @rdname assert
assert_exist <- function(file) {
  if (!file.exists(file))
    stop2("File '%s' doesn't exist.", file)
}

#' @export
#' @rdname assert
assert_noexist <- function(file) {
  if (file.exists(file))
    stop2("File '%s' already exists.", file)
}

################################################################################

#' @export
#' @rdname assert
assert_nodots <- function() {

  list_dots <- eval(parse(text = "list(...)"), parent.frame())
  if (!identical(list_dots, list())) {
    arg.name <- names(list_dots[1])
    if (is.null(arg.name)) {
      stop2("One passed argument is not used.")
    } else {
      stop2("Argument '%s' not used.", arg.name)
    }
  }
}

################################################################################

#' @export
#' @rdname assert
assert_ext <- function(file, ext) {
  if (!grepl(sprintf("\\.%s$", ext), file))
    stop2("Extension of '%s' must be '.%s'.", file, ext)
}

################################################################################

#' @export
#' @rdname assert
assert_type <- function(x, type)  {
  if (typeof(x) != type)
    stop2("'%s' is not of type '%s'.", deparse(substitute(x)), type)
}

################################################################################

#' @export
#' @rdname assert
assert_sorted <- function(x, strict = FALSE)  {
  if (is.unsorted(x, na.rm = TRUE, strictly = strict))
    stop2("'%s' is not sorted.", deparse(substitute(x)))
}

################################################################################
