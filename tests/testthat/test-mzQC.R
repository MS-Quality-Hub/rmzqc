
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

test_that("isValidMzQC = function(x, ...)", {
  suppressWarnings(## the expression below yields more than one warning, but expect_warning only catches a single one
   expect_warning(out <- isValidMzQC(MzQCcvParameter$new("MS:4000059")))
  )
  expect_false(out)

  expect_true(isValidMzQC(MzQCcvParameter$new("MS:4000059", "Number of MS1 spectra")))

  suppressWarnings(## the expression below yields more than one warning, but expect_warning only catches a single one
    expect_warning(out <- isValidMzQC(list(MzQCcvParameter$new("MS:4000059"))))
  )
  expect_false(out)

  expect_true(isValidMzQC(list(MzQCcvParameter$new("MS:4000059", "Number of MS1 spectra"))))

  suppressWarnings(## the expression below yields more than one warning, but expect_warning only catches a single one
    expect_warning(out <- isValidMzQC(list(MzQCcvParameter$new("MS:4000059", "Number of MS1 spectra")),
                                   MzQCcvParameter$new()))
  )
  expect_false(out)
})


test_that("fromDatatoMzQC = function(mzqc_class, data)", {
  data = MzQCcvParameter$new("acc", "myName", "value")
  data_recovered = fromDatatoMzQC(MzQCcvParameter, list(jsonlite::fromJSON(jsonlite::toJSON(data))))
  expect_equal(data_recovered[[1]]$accession, data$accession)
  expect_equal(data_recovered[[1]]$value, data$value)
  expect_equal(data_recovered[[1]]$description, data$description)
})
