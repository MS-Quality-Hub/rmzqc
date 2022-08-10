##
## Author: Chris Bielow
##

test_that("fileSuffixToCV = function(filepath)", {
  expect_true(fileSuffixToCV("test.mZmL") == "MS:1000584")
  expect_true(fileSuffixToCV("test.mZmL") == "MS:1000563")
  expect_true(all(fileSuffixToCV(c("test.raw", "bla.mzML")) == c("MS:1000563", "MS:1000584")))
})
