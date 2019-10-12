################################################################################

context("test-assert")

################################################################################

test_that("assert_nona() works", {
  expect_null(assert_nona(1:3))
  expect_error(assert_nona(c(1:3, NA)), "You can't have missing values in ")
})

test_that("assert_args() works", {
  expect_null(assert_args(assert_nona, "x"))
  expect_error(assert_args(assert_nona, "x2"),
               "'assert_nona' should have argument named 'x2'.")
  expect_error(assert_args(assert_nona, c("x", "x2")),
               "'assert_nona' should have arguments named 'x, x2.")

  assert_args(cbind, '...')
  expect_error(assert_args(cbind()), "'cbind()' is not a function.", fixed = TRUE)
})

test_that("assert_lengths() works", {
  expect_null(assert_lengths(1:3, 4:6, as.list(1:3)))
  expect_error(assert_lengths(1:3, 4:5, as.list(1:3)),
               "Incompatibility between dimensions.")
  expect_error(assert_lengths(1:3, 4:6, as.list(1:2)),
               "Incompatibility between dimensions.")
  expect_error(assert_lengths(as.list(1:3)),
               "You should check the lengths of at least two elements.")
})

test_that("assert_int() works", {
  expect_null(assert_int(NULL))
  expect_null(assert_int(c(1, 2, 3)))
  expect_null(assert_int(c(1, 2, 3, NA)))
  expect_error(assert_int(c(1, 2, 3, 3.5)), " should contain only integers.")
  expect_error(assert_int(letters), "'letters' should contain only integers.")
})

test_that("assert_one_int() works", {
  expect_error(assert_one_int(NULL), "'NULL' should be of length 1.")
  expect_error(assert_one_int(c(1, 2, 3)), " should be of length 1.")
  expect_null(assert_one_int(1L))
  expect_null(assert_one_int(1))
  expect_error(assert_one_int(NA), " should be an integer.")
  expect_error(assert_one_int(1.5), " should be an integer.")
})

test_that("assert_pos() works", {
  expect_null(assert_pos(NULL))
  expect_null(assert_pos(c(1, 2, 3)))
  expect_error(assert_pos(c(NA, 1, 2, 3)), " should have only positive values.")
  expect_error(assert_pos(c(0, 1, 2, 3)), " should have only positive values.")
  expect_null(assert_pos(c(0, 1, 2, 3), strict = FALSE))
  expect_error(assert_int(c(1, 2, 3, 3.5)), " should contain only integers.")
  expect_error(assert_pos(letters), "'letters' should be numeric.")
})

test_that("assert_01() works", {
  expect_null(assert_01(0:1))
  expect_null(assert_01(c(0, 1, 0)))
  expect_error(assert_01(c(0, 1, 0, 2)), " should be composed of 0s and 1s.")
  expect_error(assert_01(c(TRUE, FALSE)), " should be composed of 0s and 1s.")
})

test_that("assert_multiple() works", {
  expect_null(assert_multiple(1:3))
  expect_warning(
    assert_multiple(c(1, 2, 1, 2)),
    "'c(1, 2, 1, 2)' is composed of only two different levels.", fixed = TRUE)
  expect_error(
    assert_multiple(rep(1, 3)),
    "'rep(1, 3)' should be composed of different values.", fixed = TRUE)
  x <- c(1, 2, 1, 2)
  expect_warning(
    assert_multiple(x),
    "'x' is composed of only two different levels.", fixed = TRUE)
  y <- rep(1, 3)
  expect_error(
    assert_multiple(y),
    "'y' should be composed of different values.", fixed = TRUE)
})

test_that("assert_class() works", {
  expect_null(assert_class(assert_nona, "function"))
  expect_error(assert_class(assert_nona, "lm"),
               "'assert_nona' is not of class 'lm'.")
  x <- NULL
  expect_error(assert_class(x, 'lm'), "'x' is not of class 'lm'.")
  expect_null(assert_class_or_null(assert_nona, "function"))
  expect_error(assert_class_or_null(assert_nona, "lm"),
               "'assert_nona' is not 'NULL' or of class 'lm'.")
  expect_null(assert_class_or_null(x, "lm"))
})

test_that("assert_all() works", {
  expect_null(assert_all(1:3 > 0))
  expect_null(assert_all(rep(5, 4), 5))
  expect_error(
    assert_all(rep(5, 4), 4),
    "At least one value of 'rep(5, 4)' is different from '4'.", fixed = TRUE)
})

test_that("assert_dir() works", {
  expect_null(assert_dir(tempdir()))
  tmp <- tempfile()
  expect_message(
    assert_dir(tmp),
    sprintf("Creating directory \"%s\" which didn't exist..", tmp), fixed = TRUE)
  expect_true(dir.exists(tmp))
  tmp2 <- file.path(tmp, "too", "far")
  expect_error(
    assert_dir(tmp2),
    sprintf("Problem creating directory \"%s\". Recursive path?", tmp2),
    fixed = TRUE)
})

test_that("assert_exist() works", {
  tmp <- tempfile()
  expect_error(assert_exist(tmp), sprintf("File '%s' doesn't exist.", tmp),
               fixed = TRUE)
  expect_null(assert_noexist(tmp))
  write("test", tmp)
  expect_null(assert_exist(tmp))
  expect_error(assert_noexist(tmp), sprintf("File '%s' already exists.", tmp),
               fixed = TRUE)
})

test_that("assert_nodots() works", {
  test <- function(a = 1, ...) {
    assert_nodots()
    NULL
  }
  expect_null(test())
  expect_null(test(1:2))
  expect_error(test(1:2, 1:3), "One passed argument is not used.")
  expect_error(test(b = 1:3), "Argument 'b' not used.")
})

test_that("assert_ext() works", {
  expect_null(assert_ext("test.txt", "txt"))
  expect_error(assert_ext("test.txt", "csv"),
               "Extension of 'test.txt' must be '.csv'.")
})

test_that("assert_type() works", {
  expect_null(assert_type(1:3, "integer"))
  expect_error(assert_type(1:3, "double"), "'1:3' is not of type 'double'.")
})

test_that("assert_sorted() works", {
  expect_null(assert_sorted(NULL))
  expect_null(assert_sorted(1:3))
  expect_null(assert_sorted(c(1:3, NA)))
  expect_null(assert_sorted(c(1:3, 3)))
  expect_error(assert_sorted(c(1:3, 3), strict = TRUE),
               "'c(1:3, 3)' is not sorted.", fixed = TRUE)
})

################################################################################
