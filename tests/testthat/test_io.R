##
## Author: Chris Bielow
##

test_that("readMZQC and writeMZQC", {
  fp = system.file("testdata", "test.mzQC", package="rmzqc")
  fp
  mq = readMZQC(fp)
  f.mq = tempfile(pattern = "tmp", fileext = ".mzQC");
  writeMZQC(f.mq, mq)
  mq2 = readMZQC(f.mq)
  ## cannot compare mqQC objects. But JSON can...
  j.mq = jsonlite::toJSON(mq)
  j.mq2 = jsonlite::toJSON(mq2)

  expect_true(j.mq == j.mq2)
})


test_that("readMZQC and writeMZQC - check for extra data in mzQC file and warn", {
  fp = system.file("testdata", "test.mzQC", package="rmzqc")
  fp
  json_raw = jsonlite::read_json(fp)
  ## add extra data, which is out-of-spec
  json_raw$mzQC$newData = list("one", 2)
  json_raw$mzQC$runQualities[[1]]$metadata$stuff = 1:5
  ## test the warning messages(see tests/thestthat/_snaps/io.md)
  expect_snapshot(
    readMZQCFromJSON(json_raw),
    cran = TRUE
  )
})
