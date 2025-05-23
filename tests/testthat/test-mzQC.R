
test_that("isValidMzQC = function(x, parent_context = NULL)", {
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

  # Test with multiple objects (now needs to be done separately)
  valid_param <- MzQCcvParameter$new("MS:4000059", "Number of MS1 spectra")
  invalid_param <- MzQCcvParameter$new()
  
  # Check each object individually
  expect_true(isValidMzQC(valid_param))
  suppressWarnings(
    expect_warning(out <- isValidMzQC(invalid_param))
  )
  expect_false(out)
})


test_that("fromDatatoMzQC = function(mzqc_class, data)", {
  data = rmzqc::MzQCcvParameter$new("acc", "myName", "value")
  data_recovered = rmzqc::fromDatatoMzQC(rmzqc::MzQCcvParameter, list(jsonlite::fromJSON(jsonlite::toJSON(data))))
  expect_equal(data_recovered[[1]]$accession, data$accession)
  expect_equal(data_recovered[[1]]$value, data$value)
  expect_equal(data_recovered[[1]]$description, data$description)
})


test_that("fromDatatoMzQCobj = function(mzqc_class, data)", {
  data = rmzqc::MzQCcvParameter$new("acc", "myName", "value")
  data_recovered = rmzqc::fromDatatoMzQCobj(rmzqc::MzQCcvParameter, jsonlite::fromJSON(jsonlite::toJSON(data)))
  expect_equal(data_recovered$accession, data$accession)
  expect_equal(data_recovered$value, data$value)
  expect_equal(data_recovered$description, data$description)
})
