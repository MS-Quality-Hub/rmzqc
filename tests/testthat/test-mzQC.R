
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


test_that("MzQCbaseQuality::getMetric = function(.self, accession = NULL, name = NULL)", {
  data = readMZQC(system.file("./testdata/test.mzQC", package = "rmzqc", mustWork = TRUE))

  extracted_metrics = data$runQualities[[1]]$getMetric(accession = "MS:4000059")
  expect_equal(length(extracted_metrics), 1)
  expect_equal(extracted_metrics[[1]]$accession, "MS:4000059")
  expect_equal(extracted_metrics[[1]]$name, "number of MS1 spectra")

  extracted_metrics = data$runQualities[[1]]$getMetric(name = "number of MS1 spectra")
  expect_equal(length(extracted_metrics), 1)
  expect_equal(extracted_metrics[[1]]$accession, "MS:4000059")
  expect_equal(extracted_metrics[[1]]$name, "number of MS1 spectra")
})




test_that("MzQCqualityMetric::clone()", {
  ## ensure that cloning creates a true copy
  template_proteinCount = rmzqc::getQualityMetricTemplate("MS:1002406") # count of identified clusters
  clone1 = template_proteinCount$clone();
  clone2 = template_proteinCount$clone();
  clone1$value = 1;
  clone2$value = 2;
  expect_equal(clone1$value, 1)
  expect_equal(clone2$value, 2)
})
