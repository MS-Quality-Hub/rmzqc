##
## Author: Chris Bielow
##

test_that("getQualityMetricTemplate = function(accession, mzcv_dict = getCVSingleton())", {
  d1 = getQualityMetricTemplate("MS:4000059") ## number of MS1 scans
  expect_true(d1$accession == "MS:4000059")
  expect_true(d1$name == "number of MS1 spectra")
  expect_true(d1$description == "\"The number of MS1 events in the run.\" [PSI:MS]")
  expect_true(is.na(d1$value))
})


test_that("getCVTemplate = function(accession, CV = getCVSingleton())", {
  d1 = getCVTemplate("MS:1000584") ## mzML format
  expect_true(d1$accession == "MS:1000584")
  expect_true(d1$name == "mzML format")
  expect_true(is.na(d1$value))
})
