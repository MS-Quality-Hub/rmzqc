##
## Author: Chris Bielow
##

test_that("filenameToCV = function(filepath)", {
  expect_true(filenameToCV("test.mZmL") == "MS:1000584")
  expect_true(filenameToCV("test.wiff") == "MS:1000562")
  expect_true(all(filenameToCV(c("test.raw", "bla.mzML")) == c("MS:1000563", "MS:1000584")))
  expect_warning(filenameToCV("test.special") == "MS:1000584") ## generic mass spectrometer file format
})


test_that('toQCMetric = function(id, value, on_violation = c("error", "warn"))', {
  ## load the CV first, since this will print some information (i.e. not silent)
  myCV = CV_$new()
  myCV$ensureHasData()
  expect_silent(toQCMetric(id = "MS:4000059", value = 13405))
  expect_silent(toQCMetric(id = "MS:4000059", value = "13405"))
  expect_error(toQCMetric(id = "MS:4000059", value = data.frame(n = 1), on_violation = "error"))
})

