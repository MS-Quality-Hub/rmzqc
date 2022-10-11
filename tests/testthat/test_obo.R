##
## Author: Chris Bielow
##

test_that("getCVDictionary = function()", {
  cv = getCVDictionary()
  expect_true(all(c("id", "name", "def") %in% names(cv)))
  expect_true("MS:1001000" %in% cv$id)
  expect_true("gene name" %in% cv$name)
})


test_that("getDefaultCVVersion = function()", {
  expect_true(getDefaultCVVersion() == "4.1.95")
})

test_that("getCVSingleton()", {
  d1 = getCVSingleton()
  d2 = getCVSingleton()
  expect_true(identical(d1$data, d2$data))
  entry = d1$byID("MS:1000563")
  expect_true(entry$parents == "MS:1000560")
  suppressWarnings(expect_null(d1$byID("does_not_exit")))
})

