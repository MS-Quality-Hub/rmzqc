##
## Author: Chris Bielow
##

test_that("NULL_to_charNA = function(char_or_NULL)", {
  expect_true(is.na(NULL_to_charNA(NA)))
  expect_true(is.na(NULL_to_charNA(NULL)))
  expect_true(NULL_to_charNA("hi") == "hi")
})

test_that("NULL_to_NA = function(var_or_NULL)", {
  expect_true(is.na(NULL_to_NA(NA)))
  expect_true(is.na(NULL_to_NA(NULL)))
  expect_true(NULL_to_NA("hi") == "hi")
})


test_that("isUndefined = function(s, ..., verbose = TRUE)", {
  expect_true(isUndefined(NA, verbose = FALSE))
  expect_true(isUndefined(NULL, verbose = FALSE))
  expect_true(isUndefined(NA, NULL, verbose = FALSE))
  expect_false(isUndefined("", verbose = FALSE))
  expect_true(isUndefined("", NA, verbose = FALSE))
  expect_true(isUndefined(NA, "", verbose = FALSE))
  expect_false(isUndefined(1, verbose = FALSE))
  myVar = NA
  expect_warning(out <- isUndefined(myVar), "Variable 'myVar' is NA/NULL!")
  expect_true(out)
})


test_that("removeFileSuffix = function(filepath)", {
  expect_true(removeFileSuffix("test.tar.gz") == 'test.tar')
  expect_true(removeFileSuffix("test.mzML") == 'test')
  expect_true(removeFileSuffix("/path/to/test.mzML") == '/path/to/test')
  expect_true(removeFileSuffix("test_no_dot") == 'test_no_dot')
})

test_that("hasFileSuffix = function(filepath, suffix)", {
  expect_true(hasFileSuffix("bla.txt", "txt"))
  expect_true(hasFileSuffix("bla.txt", ".txt"))
  expect_true(hasFileSuffix("bla.txt", ".TXT"))
  expect_true(hasFileSuffix("fo", ""))
  expect_true(hasFileSuffix("", ""))
  expect_false(hasFileSuffix("bla.txt", "doc"))
  expect_false(hasFileSuffix("bla.txt", ".doc"))
  expect_false(hasFileSuffix("fo", ".doc"))
  expect_false(hasFileSuffix("", ".doc"))
})

test_that("check_type = function(value, any_expected_class_types, expected_length = 0)", {
  expect_true(check_type(1, "numeric", 1))
  expect_false(check_type("1", "numeric", 1))
  expect_false(check_type(1, "numeric", 2))
  expect_true(check_type("ABC", "character", 1))
  expect_true(check_type("ABC", "character"))
  expect_false(check_type("ABC", "character", 2))
  expect_true(check_type(c("ABC", "DEF"), "character", 2))
  expect_true(check_type(1.1, c("numeric", "double")))
  expect_true(check_type(1.1, c("numeric", "double"), 1))
  expect_false(check_type(1.1, c("numeric", "double"), 2))
  expect_true(check_type(c(1.1, 2.2), c("numeric", "double"), 2))
  expect_true(check_type(matrix(1:9, nrow=3), "matrix"))
  expect_true(check_type(data.frame(a=1:3, b=4:6), c("something", "data.frame")))
})
