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
