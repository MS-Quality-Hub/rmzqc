##
## Author: Chris Bielow
##

test_that("getCVDictionary = function()", {
  cv = getCVDictionary()
  expect_true(all(c("id", "name", "def") %in% names(cv)))
  expect_true("MS:1001000" %in% cv$id)
  expect_true("gene name" %in% cv$name)
})


test_that("CV_ <- R6::R6Class...", {
  d1 = CV_$new()
  d2 = CV_$new()
  expect_true(identical(d1$data, d2$data))
})

