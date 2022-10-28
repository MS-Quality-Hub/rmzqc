##
## Author: Chris Bielow
##

test_that("validateFromFile = function(filepath, verbose = TRUE)", {
  fp = system.file("testdata/test.mzQC", package="rmzqc")
  fp
  expect_true(validateFromFile(fp))
})

test_that("validateFromString = function(JSON_string, verbose = TRUE)", {
  mzQC_strings = readLines(system.file("testdata/test.mzQC", package="rmzqc"))
  res = validateFromString(mzQC_strings)
  expect_true(res)
  ## truncated content
  expect_error(validateFromString(substr(mzQC_strings, 1, 100)))
})

test_that("validateFromObj = function(mzqc_root, verbose = TRUE)", {
  root = readMZQC(system.file("testdata/test.mzQC", package="rmzqc"))
  res = validateFromObj(root)
  expect_true(res)

  ## make it invalid (this even triggers during conversion to JSON, not even reaching the validator)
  r2 = root
  r2$runQualities[[1]]$metadata$inputFiles[[1]]$location = "/invalid/uri"
  expect_error(validateFromObj(r2))

  ## make it invalid (this even triggers during conversion to JSON, not even reaching the validator)
  root$runQualities = list()
  root$setQualities = list()
  expect_error(validateFromObj(root))

  ## not an mzQC root object
  expect_error(validateFromObj(MzQCanalysisSoftware$new()))
})
